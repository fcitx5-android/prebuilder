{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Base
  ( module Development.Shake,
    module Development.Shake.Classes,
    module Development.Shake.FilePath,
    module Control.Monad.Extra,
    module System.Directory.Extra,
    Generic,
    DownloadFile (..),
    downloadFileRule,
    download,
    AndroidEnv (..),
    getSdkCMake,
    getSdkNinja,
    getABIList,
    getCMakeToolchain,
    getNdkStrip,
    withAndroidEnv,
    getAndroidEnv,
    WithAndroidEnv (..),
    getConfig',
    prebuilderVersion,
    outputDir,
    copyFileAndCreateDir,
    buildWithAndroidEnv,
    execute,
    isInGitHubActionRule,
    isInGitHubAction,
    writeGitHubBuildSummary,
    getOutputDirRule,
    getOutputDir,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (forM_, fromMaybeM, void, whenM)
import Data.List.Extra (split)
import Data.Maybe (fromJust, isJust)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Command
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics (Generic)
import System.Directory.Extra
  ( canonicalizePath,
    createDirectoryLink,
    doesPathExist,
    getCurrentDirectory,
    removePathForcibly,
  )
import qualified System.Directory.Extra as IO

prebuilderVersion :: String
prebuilderVersion = "3"

outputDir :: FilePath
outputDir = "build"

--------------------------------------------------------------------------------

data DownloadFile = DownloadFile
  { downloadBaseUrl :: String,
    downloadFileName :: FilePath,
    downloadSha256 :: String
  }
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult DownloadFile = ()

downloadFileRule :: Rules ()
downloadFileRule = addBuiltinRule noLint noIdentity $ \DownloadFile {..} _ _ -> do
  let downloadedFilePath = outputDir </> downloadFileName
  b <- liftIO $ IO.doesFileExist downloadedFilePath
  mNow <- if b then Just <$> sha256sum downloadedFilePath else pure Nothing
  case mNow of
    Just now
      | now == downloadSha256 ->
          pure $ RunResult ChangedRecomputeSame mempty ()
    _ -> do
      let url = downloadBaseUrl <> downloadFileName
      cmd_ (Cwd outputDir) "curl" "-LO" url
      sha256 <- sha256sum downloadedFilePath
      if sha256 /= downloadSha256
        then fail $ "SHA256 mismatched: expected " <> (if not $ null downloadSha256 then downloadSha256 else "[empty]") <> ", but got " <> sha256
        else pure $ RunResult ChangedRecomputeDiff mempty ()

sha256sum :: FilePath -> Action String
sha256sum file = do
  (Stdout result) <- cmd "sha256sum" file
  pure $ takeWhile (/= ' ') result

-- | Download to @outputDir@.
download :: String -> FilePath -> String -> Action FilePath
download downloadBaseUrl downloadFileName downloadSha256 = outputDir </> downloadFileName <$ apply1 DownloadFile {..}

--------------------------------------------------------------------------------

data AndroidEnv = AndroidEnv
  { sdkRoot :: FilePath,
    ndkRoot :: FilePath,
    sdkCMakeVersion :: String,
    platform :: Int,
    abi :: String
  }
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

getSdkCMake :: AndroidEnv -> FilePath
getSdkCMake AndroidEnv {..} = sdkRoot </> "cmake" </> sdkCMakeVersion </> "bin" </> "cmake"

getSdkNinja :: AndroidEnv -> FilePath
getSdkNinja AndroidEnv {..} = sdkRoot </> "cmake" </> sdkCMakeVersion </> "bin"

getABIList :: AndroidEnv -> [String]
getABIList AndroidEnv {..} = split (== ',') abi

getCMakeToolchain :: AndroidEnv -> FilePath
getCMakeToolchain AndroidEnv {..} = ndkRoot </> "build" </> "cmake" </> "android.toolchain.cmake"

getNdkStrip :: AndroidEnv -> FilePath
getNdkStrip AndroidEnv {..} = ndkRoot </> "toolchains" </> "llvm" </> "prebuilt" </> "linux-x86_64" </> "bin" </> "llvm-strip"

withAndroidEnv :: AndroidEnv -> (FilePath -> FilePath -> FilePath -> FilePath -> [String] -> Action a) -> Action a
withAndroidEnv env f = f (getSdkCMake env) (getCMakeToolchain env) (getSdkNinja env) (getNdkStrip env) (getABIList env)

getAndroidEnv :: Action AndroidEnv
getAndroidEnv = do
  sdkRoot <-
    fromMaybeM (fail "Both environment variable ANDROID_HOME and ANDROID_SDK_ROOT are unset!") $
      (<|>) <$> getEnv "ANDROID_HOME" <*> getEnv "ANDROID_SDK_ROOT"
  ndkRoot <- env "ANDROID_NDK_ROOT"
  sdkCMakeVersion <- env "CMAKE_VERSION"
  platform <- read <$> env "ANDROID_PLATFORM"
  abi <- env "ABI"
  pure AndroidEnv {..}
  where
    env name = fromMaybeM (fail $ "Environment variable " <> name <> " is unset!") $ getEnv name

data WithAndroidEnv k = WithAndroidEnv k AndroidEnv
  deriving stock (Eq, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance (Show k) => Show (WithAndroidEnv k) where
  show (WithAndroidEnv k n) = show k <> " (" <> show n <> ")"

type instance RuleResult (WithAndroidEnv k) = RuleResult k

buildWithAndroidEnv :: (WithAndroidEnv k -> Action a) -> k -> Action a
buildWithAndroidEnv f k = getAndroidEnv >>= f . WithAndroidEnv k

--------------------------------------------------------------------------------
getConfig' :: String -> Action String
getConfig' x = fromJust <$> getConfig x

copyFileAndCreateDir :: FilePath -> FilePath -> IO ()
copyFileAndCreateDir src dst = do
  IO.createDirectoryIfMissing True $ takeDirectory dst
  IO.copyFile src dst

--------------------------------------------------------------------------------

execute :: String -> (CmdArguments args) => args :-> Action ()
execute tool =
  cmdArguments
    ( CmdArgument
        [ Right (outputDir </> "bin" </> tool),
          Left (AddEnv "LD_LIBRARY_PATH" (outputDir </> "lib"))
        ]
    )

newtype IsInGitHubAction = IsInGitHubAction ()
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult IsInGitHubAction = Bool

isInGitHubActionRule :: Rules ()
isInGitHubActionRule = void $ addOracle $ \(IsInGitHubAction _) -> isJust <$> getEnv "GITHUB_ACTIONS"

isInGitHubAction :: Action Bool
isInGitHubAction = askOracle $ IsInGitHubAction ()

-- Note: Check if we are in GitHub Actions before using this function.
writeGitHubBuildSummary :: [String] -> Action ()
writeGitHubBuildSummary summary = do
  path <- fromJust <$> getEnv "GITHUB_STEP_SUMMARY"
  liftIO $ appendFile path $ unlines summary

--------------------------------------------------------------------------------

newtype GetOutputDir = GetOutputDir ()
  deriving (Generic, Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult GetOutputDir = FilePath

getOutputDirRule :: Rules ()
getOutputDirRule = void $ addOracle $ \(GetOutputDir _) -> liftIO $ canonicalizePath outputDir

getOutputDir :: Action FilePath
getOutputDir = askOracle $ GetOutputDir ()

--------------------------------------------------------------------------------
