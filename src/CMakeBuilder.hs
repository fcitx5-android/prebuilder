{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module CMakeBuilder
  ( BuildEnv (..),
    BuildActionABI (..),
    BuildAction (..),
    CmakeBuilder (..),
    cmakeBuilder,
    useCMake,
  )
where

import Base
import Control.Exception.Extra (Partial, try)
import Control.Monad (when)
import Data.Maybe (maybeToList)
import System.Directory.Extra (removeDirectoryRecursive)
import System.Exit (ExitCode (..))

-- | Environment for running cmake for each abi
data BuildEnv = BuildEnv
  { buildEnvSrc :: FilePath,
    -- | canonicalized @outputDir@
    buildEnvOut :: FilePath,
    buildEnvAndroid :: AndroidEnv,
    buildEnvABI :: String,
    buildEnvOutPrefix :: FilePath,
    buildEnvBuildDir :: FilePath
  }

-- | Args are key and build env
newtype BuildActionABI q = BuildActionABI {unBuildActionABI :: q -> BuildEnv -> Action ()}

instance Semigroup (BuildActionABI q) where
  BuildActionABI a <> BuildActionABI b = BuildActionABI $ \q env -> a q env >> b q env

instance Monoid (BuildActionABI q) where
  mempty = BuildActionABI $ \_ _ -> pure ()

-- | Args are key and source
newtype BuildAction q = BuildAction {unBuildAction :: q -> FilePath -> Action ()}

instance Semigroup (BuildAction q) where
  BuildAction a <> BuildAction b = BuildAction $ \q src -> a q src >> b q src

instance Monoid (BuildAction q) where
  mempty = BuildAction $ \_ _ -> pure ()

data CmakeBuilder q = CmakeBuilder
  { -- | name of the library
    name :: String,
    -- | output dir -> source dir
    -- builder can download source code here
    source :: FilePath -> Action FilePath,
    -- | additional cmake flags
    cmakeFlags :: BuildEnv -> [String],
    -- | action before cmake, after source
    preBuild :: BuildAction q,
    -- | action for each abi before cmake
    preBuildEachABI :: BuildActionABI q,
    -- | action for each abi after cmake
    postBuildEachABI :: BuildActionABI q,
    -- | action after cmake
    postBuild :: BuildAction q,
    -- | maybe library need to install using it own way
    doInstall :: Bool,
    -- | path the cmake file
    -- if not specified, run cmake in source dir
    cmakeFile :: Maybe FilePath
  }

cmakeBuilder :: String -> CmakeBuilder b
cmakeBuilder name =
  CmakeBuilder
    { preBuild = mempty,
      preBuildEachABI = mempty,
      postBuildEachABI = mempty,
      postBuild = mempty,
      cmakeFlags = mempty,
      source = const $ pure name,
      name = name,
      doInstall = True,
      cmakeFile = Nothing
    }

useCMake ::
  (q ~ WithAndroidEnv q', RuleResult q ~ a, a ~ (), ShakeValue q, ShakeValue a, Partial) =>
  CmakeBuilder q' ->
  Rules (q -> Action a)
useCMake CmakeBuilder {..} = addOracle $ \(WithAndroidEnv q env) -> do
  out <- getOutputDir
  src <- source out
  let buildEnv abi =
        BuildEnv
          { buildEnvSrc = src,
            buildEnvOut = out,
            buildEnvAndroid = env,
            buildEnvABI = abi,
            buildEnvOutPrefix = out </> name </> abi,
            buildEnvBuildDir = out </> name <> "-build-" <> abi
          }
  unBuildAction preBuild q src
  withAndroidEnv env $ \cmake toolchain ninja _strip abiList ->
    forM_ abiList $ \a -> do
      let bEnv@BuildEnv {..} = buildEnv a
      unBuildActionABI preBuildEachABI q bEnv
      cmd_
        (Cwd src)
        (AddPath [] [ ninja ])
        cmake
        "-B"
        buildEnvBuildDir
        "-GNinja"
        ( [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show (platform env),
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> buildEnvOutPrefix,
            "-DCMAKE_BUILD_TYPE=Release"
          ]
            <> cmakeFlags bEnv
        )
        (maybeToList cmakeFile)
      cmd_ (Cwd src) cmake "--build" buildEnvBuildDir
      when doInstall $
        cmd_ (Cwd src) cmake "--install" buildEnvBuildDir
      unBuildActionABI postBuildEachABI q bEnv
      -- for all binaries
      libs <- liftIO $ getDirectoryFilesIO buildEnvOutPrefix ["//*.a"]
      forM_ libs $ \lib -> do
        -- strip
        cmd_ (Cwd buildEnvOutPrefix) (getNdkStrip buildEnvAndroid) "--strip-unneeded" lib
        -- detect hardcoded path
        (Exit c, Stdout result) <- cmd (Cwd buildEnvOutPrefix) Shell "strings --all --bytes=8" lib "| grep prebuilder"
        let libPath = makeRelative out (buildEnvOutPrefix </> lib)
        when (c == ExitSuccess) $ do
          github <- isInGitHubAction
          if github
            then
              writeGitHubBuildSummary
                [ "Hardcoded paths in `" <> libPath <> "`:",
                  "<details>",
                  "<summary>(expand for details)</summary>",
                  "\n",
                  "```",
                  result,
                  "```",
                  "\n",
                  "</details>",
                  "\n"
                ]
            else
              putWarn $
                "Hardcoded paths in '"
                  <> libPath
                  <> "':\n"
                  <> result
        -- remove pkg-config and bin
        liftIO $
          do
            void $ try @IOError $ removeDirectoryRecursive $ buildEnvOutPrefix </> "lib/pkgconfig"
            void $ try @IOError $ removeDirectoryRecursive $ buildEnvOutPrefix </> "bin"
  unBuildAction postBuild q src
