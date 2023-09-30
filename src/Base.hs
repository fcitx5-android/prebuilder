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
  )
where

import Control.Monad.Extra (forM_, fromMaybeM, whenM)
import qualified Data.ByteString.Char8 as BS
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
import System.IO.Unsafe (unsafePerformIO)

prebuilderVersion :: String
prebuilderVersion = "3"

{-# NOINLINE outputDir #-}
outputDir :: FilePath
outputDir = unsafePerformIO $ canonicalizePath "_build"

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
downloadFileRule = addBuiltinRule noLint noIdentity $ \DownloadFile {..} mOld mode -> do
  b <- liftIO $ IO.doesFileExist downloadFileName
  mNow <- if b then Just <$> sha256sum downloadFileName else pure Nothing
  case mNow of
    Just now
      | mode == RunDependenciesSame,
        now == downloadSha256,
        Just (BS.unpack -> old) <- mOld,
        old == now -> do
          pure $ RunResult ChangedNothing (BS.pack now) ()
    _ -> do
      let url = downloadBaseUrl <> downloadFileName
      cmd_ "curl" "-LO" url
      sha256 <- sha256sum downloadFileName
      if sha256 /= downloadSha256
        then fail $ "SHA256 mismatched: expected " <> (if not $ null downloadSha256 then downloadSha256 else "[empty]") <> ", but got " <> sha256
        else pure $ RunResult ChangedRecomputeDiff (BS.pack sha256) ()

sha256sum :: FilePath -> Action String
sha256sum file = do
  (Stdout result) <- cmd "sha256sum" file
  pure $ takeWhile (/= ' ') result

download :: String -> FilePath -> String -> Action ()
download downloadBaseUrl downloadFileName downloadSha256 = apply1 DownloadFile {..}

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
  sdkRoot <- env "ANDROID_SDK_ROOT"
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
