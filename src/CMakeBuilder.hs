{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module CMakeBuilder
  ( BuildEnv (..),
    BuildActionABI (..),
    BuildAction (..),
    CmakeBuilder (..),
    cmakeBuilder,
    useCMake,
    stripLib,
    removePkgConfig,
    removeBin,
  )
where

import Base
import Control.Exception.Extra (Partial)
import Control.Monad (when)
import Data.Maybe (maybeToList)

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
  out <- liftIO $ canonicalizePath outputDir
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
        cmake
        "-B"
        buildEnvBuildDir
        "-GNinja"
        ( [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DCMAKE_MAKE_PROGRAM=" <> ninja,
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
  unBuildAction postBuild q src

stripLib :: FilePath -> BuildActionABI q
stripLib path = BuildActionABI $ \_ BuildEnv {..} ->
  cmd_
    (Cwd buildEnvOutPrefix)
    Shell
    (getNdkStrip buildEnvAndroid)
    "--strip-unneeded"
    path

removePkgConfig :: BuildActionABI q
removePkgConfig = BuildActionABI $ \_ BuildEnv {..} ->
  removeFilesAfter buildEnvOutPrefix ["lib/pkgconfig"]

removeBin :: BuildActionABI q
removeBin = BuildActionABI $ \_ BuildEnv {..} ->
  removeFilesAfter buildEnvOutPrefix ["bin"]
