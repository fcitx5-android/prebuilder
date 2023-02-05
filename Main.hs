{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_, void)
import Control.Monad.Extra (fromMaybeM, whenM)
import Data.ByteString.Char8 qualified as BS
import Data.List.Extra (find, replace, split)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics (Generic)
import GHC.Stack
import System.Directory.Extra
import System.Directory.Extra qualified as IO

--------------------------------------------------------------------------------

prebuilderVersion :: String
prebuilderVersion = "2"

main :: IO ()
main = do
  mp <- getMainPath >>= canonicalizePath . takeDirectory
  shakeArgs shakeOptions {shakeReport = ["report.html"], shakeVersion = prebuilderVersion} $ do
    usingConfigFile $ mp </> "build.cfg"
    mainPathRule
    downloadFileRule
    spellDictRule
    libimeRule
    chineseAddonsRule
    fmtRule
    libeventRule
    libintlLiteRule
    luaRule
    boostRule
    "everything"
      ~> need
        [ "spell-dict",
          "libime",
          "fmt",
          "chinese-addons-data",
          "libevent",
          "libintl-lite",
          "lua",
          "boost"
        ]

fcitxDataUrl :: String
fcitxDataUrl = "https://download.fcitx-im.org/data/"

--------------------------------------------------------------------------------

spellDictRule :: Rules ()
spellDictRule = do
  "en_dict.txt" %> \out -> do
    src <- getConfig' "en_dict"
    sha256 <- getConfig' "en_dict_sha256"
    download fcitxDataUrl src sha256
    cmd_ "tar" "xf" src out
  "en_dict.fscd" %> \out -> do
    let src = "en_dict.txt"
    need [src]
    compSpellDict <- getEnvWithDefault "/usr/lib/fcitx5/libexec/comp-spell-dict" "COMP_SPELL_DICT"
    cmd_ compSpellDict "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' "en_dict.fscd" $ "spell-dict" </> "en_dict.fscd"

--------------------------------------------------------------------------------
dictNames :: [String]
dictNames = ["sc", "extb"]

tableDictNames :: [String]
tableDictNames = ["db", "erbi", "qxm", "wanfeng", "wbpy", "wbx", "zrm", "cj"]

libimeRule :: Rules ()
libimeRule = do
  lmRule
  dictRule
  tableDictRule
  "libime" ~> do
    copyFile' "sc.dict" $ "libime" </> "data" </> "sc.dict"
    copyFile' "extb.dict" $ "libime" </> "data" </> "extb.dict"
    copyFile' "sc.lm" $ "libime" </> "data" </> "zh_CN.lm"
    copyFile' "sc.lm.predict" $ "libime" </> "data" </> "zh_CN.lm.predict"
    forM_ tableDictNames $ \table ->
      let name = table <.> "main.dict"
       in copyFile' name ("libime" </> "table" </> name)

lmRule :: Rules ()
lmRule = do
  "lm_sc.arpa" %> \out -> do
    src <- getConfig' "lm_sc"
    sha256 <- getConfig' "lm_sc_sha256"
    download fcitxDataUrl src sha256
    cmd_ "tar" "xf" src out
  "sc.lm" %> \out -> do
    let src = "lm_sc.arpa"
    need [src]
    cmd_ "libime_slm_build_binary -s -a 22 -q 8 trie" src out
  "sc.lm.predict" %> \out -> do
    let src1 = "sc.lm"
        src2 = "lm_sc.arpa"
    need [src1, src2]
    cmd_ "libime_prediction" src1 src2 out

dictRule :: Rules ()
dictRule = do
  ("dict_" <>) . (<.> "txt") <$> dictNames |%> \out -> do
    src <- getConfig' "dict"
    sha256 <- getConfig' "dict_sha256"
    download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" src out
    produces $ lines txt
  (<.> "dict") <$> dictNames |%> \out -> do
    let src = "dict_" <> takeWhile (/= '.') out <.> "txt"
    need [src]
    cmd_ "libime_pinyindict" src out

tableDictRule :: Rules ()
tableDictRule = do
  (<.> "txt") <$> tableDictNames |%> \out -> do
    src <- getConfig' "table"
    sha256 <- getConfig' "table_sha256"
    download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" src out
    produces $ lines txt
  (<.> "main.dict") <$> tableDictNames |%> \out -> do
    let src = takeWhile (/= '.') out <.> "txt"
    need [src]
    cmd_ "libime_tabledict" src out

--------------------------------------------------------------------------------

chineseAddonsRule :: Rules ()
chineseAddonsRule = do
  pinyinDictRule
  pinyinStrokeRule
  pinyinTableRule
  "chinese-addons-data" ~> do
    copyFile' "emoji.dict" $ "chinese-addons-data" </> "pinyin" </> "emoji.dict"
    copyFile' "chaizi.dict" $ "chinese-addons-data" </> "pinyin" </> "chaizi.dict"
    copyFile' "py_table.mb" $ "chinese-addons-data" </> "pinyinhelper" </> "py_table.mb"
    copyFile' "py_stroke.mb" $ "chinese-addons-data" </> "pinyinhelper" </> "py_stroke.mb"

pinyinDictRule :: Rules ()
pinyinDictRule = do
  let dict name =
        name <.> "dict" %> \out -> do
          let src = name <.> "txt"
          chineseAddonsRepoDataUrl <- getConfig' "chinese_addon_repo"
          sha256 <- getConfig' $ name <> "_sha256"
          download chineseAddonsRepoDataUrl src sha256
          cmd_ "libime_pinyindict" src out
  dict "chaizi"
  dict "emoji"

pinyinStrokeRule :: Rules ()
pinyinStrokeRule = do
  "py_stroke.mb" %> \out -> do
    src <- getConfig' "py_stroke"
    sha256 <- getConfig' "py_stroke_sha256"
    download fcitxDataUrl src sha256
    cmd_ "tar" "xf" src out

pinyinTableRule :: Rules ()
pinyinTableRule = do
  "py_table.mb" %> \out -> do
    src <- getConfig' "py_table"
    sha256 <- getConfig' "py_table_sha256"
    download fcitxDataUrl src sha256
    cmd_ "tar" "xf" src out

--------------------------------------------------------------------------------

newtype Boost = Boost {boostLib :: String}
  deriving stock (Show, Typeable, Eq, Generic)
  deriving newtype (Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <- addOracle $ \(WithAndroidEnv Boost {..} AndroidEnv {..}) -> do
    boostAndroidSrc <- getCanonicalizedRootSrc "Boost-for-Android"
    boostVersion <- getConfig' "boost_version"
    sha256 <- getConfig' "boost_sha256"
    let boostTar = "boost_" <> replace "." "_" boostVersion <.> "tar" <.> "bz2"
        boostUrl = "https://boostorg.jfrog.io/artifactory/main/release/" <> boostVersion <> "/source/"
    download boostUrl boostTar sha256
    cmd_
      (boostAndroidSrc </> "build-android.sh")
      [ "--boost=" <> boostVersion,
        "--with-libraries=" <> boostLib,
        "--arch=" <> abi,
        "--target-version=" <> show platform,
        "--layout="
      ]
      ndkRoot
  "boost" ~> do
    env <- getAndroidEnv
    -- since header files are the same regardless of abi
    -- we take a random one
    let abiList = getABIList env
        firstAbi = head abiList
    -- magic dependency
    _ <- buildBoost $ WithAndroidEnv (Boost "filesystem,iostreams,regex") env
    getDirectoryFiles
      ("build" </> "out" </> firstAbi </> "include" </> "boost")
      ["//*"]
      >>= mapM_ (\x -> copyFile' ("build" </> "out" </> firstAbi </> "include" </> "boost" </> x) $ "boost" </> "include" </> "boost" </> x)
    forM_ abiList $ \a -> do
      getDirectoryFiles
        ("build" </> "out" </> a </> "lib")
        ["*.a", "//*.cmake"]
        >>= mapM_ (\x -> copyFile' ("build" </> "out" </> a </> "lib" </> x) $ "boost" </> a </> "lib" </> x)
      -- symlink headers for each abi to reduce size
      let path = "boost" </> a </> "include"
      liftIO $ whenM (doesPathExist path) $ removePathForcibly path
      liftIO $ createDirectoryLink (".." </> "include") path

--------------------------------------------------------------------------------

data Fmt = Fmt
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Fmt = ()

fmtRule :: Rules ()
fmtRule = do
  buildFmt <- addOracle $ \(WithAndroidEnv Fmt env@AndroidEnv {..}) -> do
    fmtSrc <- getCanonicalizedRootSrc "fmt"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "fmt" </> a
        let buildDir = "build-" <> a
        cmd_
          (Cwd fmtSrc)
          cmake
          "-B"
          buildDir
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_CXX_FLAGS=-std=c++17",
            "-DFMT_TEST=OFF",
            "-DFMT_DOC=OFF"
          ]
        cmd_ (Cwd fmtSrc) cmake "--build" buildDir
        cmd_ (Cwd fmtSrc) cmake "--build" buildDir "--target" "install"
        removeFilesAfter outPrefix ["//*.py", "//*.pc"]
  "fmt" ~> do
    env <- getAndroidEnv
    buildFmt $ WithAndroidEnv Fmt env

--------------------------------------------------------------------------------

data Libevent = Libevent
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Libevent = ()

libeventRule :: Rules ()
libeventRule = do
  buildLibevent <- addOracle $ \(WithAndroidEnv Libevent env@AndroidEnv {..}) -> do
    libeventSrc <- getCanonicalizedRootSrc "libevent"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let toolchain = getCmakeToolchain env
    -- make cmake generate relative _IMPORT_PREFIX
    cmd_ (Cwd libeventSrc) "sed" "-i" "1456s|${CMAKE_INSTALL_PREFIX}/||" "CMakeLists.txt"
    cmd_ (Cwd libeventSrc) "sed" "-i" "1475{\\|\"${PROJECT_SOURCE_DIR}/include\"|d}" "CMakeLists.txt"
    cmd_ (Cwd libeventSrc) "sed" "-i" "1475s|${PROJECT_BINARY_DIR}/||" "CMakeLists.txt"
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libevent" </> a
        let buildDir = "build-" <> a
        cmd_
          (Cwd libeventSrc)
          cmake
          "-B"
          buildDir
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_BUILD_TYPE=Release",
            "-DEVENT__LIBRARY_TYPE=STATIC",
            "-DEVENT__DISABLE_DEBUG_MODE=ON",
            "-DEVENT__DISABLE_THREAD_SUPPORT=ON",
            "-DEVENT__DISABLE_OPENSSL=ON",
            "-DEVENT__DISABLE_BENCHMARK=ON",
            "-DEVENT__DISABLE_TESTS=ON",
            "-DEVENT__DISABLE_REGRESS=ON",
            "-DEVENT__DISABLE_SAMPLES=ON"
          ]
        cmd_ (Cwd libeventSrc) cmake "--build" buildDir
        -- avoid void installing pkgconf files and python scripts
        cmd_ (Cwd libeventSrc) cmake "--install" buildDir "--component" "lib"
        cmd_ (Cwd libeventSrc) cmake "--install" buildDir "--component" "dev"
  "libevent" ~> do
    env <- getAndroidEnv
    buildLibevent $ WithAndroidEnv Libevent env

--------------------------------------------------------------------------------

data LibintlLite = LibintlLite
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibintlLite = ()

libintlLiteRule :: Rules ()
libintlLiteRule = do
  buildLibintlLite <- addOracle $ \(WithAndroidEnv LibintlLite env@AndroidEnv {..}) -> do
    libintlSrc <- getCanonicalizedRootSrc "libintl-lite"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libintl-lite" </> a
        let buildDir = "build-" <> a
        cmd_
          (Cwd libintlSrc)
          cmake
          "-B"
          buildDir
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_BUILD_TYPE=Release"
          ]
        cmd_ (Cwd libintlSrc) cmake "--build" buildDir
        cmd_ (Cwd libintlSrc) cmake "--build" buildDir "--target" "install"
        removeFilesAfter outPrefix ["//*.py", "//*.pc"]
  "libintl-lite" ~> do
    env <- getAndroidEnv
    buildLibintlLite $ WithAndroidEnv LibintlLite env

--------------------------------------------------------------------------------

data Lua = Lua
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Lua = ()

luaRule :: Rules ()
luaRule = do
  buildLua <- addOracle $ \(WithAndroidEnv Lua env@AndroidEnv {..}) -> do
    luaSrc <- getCanonicalizedRootSrc "Lua"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "lua" </> a
        let buildDir = "build-" <> a
        cmd_
          (Cwd luaSrc)
          cmake
          "-B"
          buildDir
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DLUA_BUILD_BINARY=OFF",
            "-DLUA_BUILD_COMPILER=OFF"
          ]
        cmd_ (Cwd luaSrc) cmake "--build" buildDir
        cmd_ (Cwd luaSrc) cmake "--build" buildDir "--target" "install"
        removeFilesAfter outPrefix ["//*.py", "//*.pc"]
  "lua" ~> do
    env <- getAndroidEnv
    buildLua $ WithAndroidEnv Lua env

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

getABIList :: AndroidEnv -> [String]
getABIList AndroidEnv {..} = split (== ',') abi

getCmakeToolchain :: AndroidEnv -> FilePath
getCmakeToolchain AndroidEnv {..} = ndkRoot </> "build" </> "cmake" </> "android.toolchain.cmake"

withAndroidEnv :: AndroidEnv -> (FilePath -> [String] -> Action a) -> Action a
withAndroidEnv env f = f (getSdkCmake env) (getABIList env)

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

instance Show k => Show (WithAndroidEnv k) where
  show (WithAndroidEnv k n) = show k <> " (" <> show n <> ")"

type instance RuleResult (WithAndroidEnv k) = RuleResult k

--------------------------------------------------------------------------------

data MainPath = MainPath
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult MainPath = FilePath

getCanonicalizedRootSrc :: FilePath -> Action FilePath
getCanonicalizedRootSrc fp = do
  root <- askOracle MainPath
  liftIO . canonicalizePath $ root </> fp

mainPathRule :: Rules ()
mainPathRule = void $
  addOracleCache $
    \MainPath -> takeDirectory <$> liftIO getMainPath

getMainPath :: HasCallStack => IO FilePath
getMainPath =
  withFrozenCallStack
    $ canonicalizePath
      . srcLocFile
      . snd
      . fromJust
      . find ((== "getMainPath") . fst)
      . getCallStack
    $ callStack

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
getConfig' :: String -> Action String
getConfig' x = fromJust <$> getConfig x

--------------------------------------------------------------------------------
