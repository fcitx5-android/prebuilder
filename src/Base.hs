{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
    getSdkCmake,
    getSdkNinja,
    getABIList,
    getCmakeToolchain,
    getNdkStrip,
    withAndroidEnv,
    getAndroidEnv,
    WithAndroidEnv (..),
    getConfig',
    prebuilderVersion,
    outputDir,
    copyFileAndCreateDir,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (forM_, fromMaybeM, whenM)
import Data.List.Extra (split)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes
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
    sdkCmakeVersion :: String,
    platform :: Int,
    abi :: String
  }
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

getSdkCmake :: AndroidEnv -> FilePath
getSdkCmake AndroidEnv {..} = sdkRoot </> "cmake" </> sdkCmakeVersion </> "bin" </> "cmake"

getSdkNinja :: AndroidEnv -> FilePath
getSdkNinja AndroidEnv {..} = sdkRoot </> "cmake" </> sdkCmakeVersion </> "bin" </> "ninja"

getABIList :: AndroidEnv -> [String]
getABIList AndroidEnv {..} = split (== ',') abi

getCmakeToolchain :: AndroidEnv -> FilePath
getCmakeToolchain AndroidEnv {..} = ndkRoot </> "build" </> "cmake" </> "android.toolchain.cmake"

getNdkStrip :: AndroidEnv -> FilePath
getNdkStrip AndroidEnv {..} = ndkRoot </> "toolchains" </> "llvm" </> "prebuilt" </> "linux-x86_64" </> "bin" </> "llvm-strip"

withAndroidEnv :: AndroidEnv -> (FilePath -> FilePath -> FilePath -> FilePath -> [String] -> Action a) -> Action a
withAndroidEnv env f = f (getSdkCmake env) (getCmakeToolchain env) (getSdkNinja env) (getNdkStrip env) (getABIList env)

getAndroidEnv :: Action AndroidEnv
getAndroidEnv = do
  sdkRoot <-
    fromMaybeM (fail "Both environment variable ANDROID_HOME and ANDROID_SDK_ROOT are unset!") $
      (<|>) <$> getEnv "ANDROID_HOME" <*> getEnv "ANDROID_SDK_ROOT"
  ndkRoot <- env "ANDROID_NDK_ROOT"
  sdkCmakeVersion <- env "CMAKE_VERSION"
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

--------------------------------------------------------------------------------
getConfig' :: String -> Action String
getConfig' x = fromJust <$> getConfig x

copyFileAndCreateDir :: FilePath -> FilePath -> IO ()
copyFileAndCreateDir src dst = do
  IO.createDirectoryIfMissing True $ takeDirectory dst
  IO.copyFile src dst
