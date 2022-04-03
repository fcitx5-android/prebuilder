{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_, void)
import Control.Monad.Extra (fromMaybeM, whenM)
import Control.Monad.Trans.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List.Extra (find, replace, split)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Rule
import GHC.Generics (Generic)
import GHC.Stack
import System.Directory.Extra
import qualified System.Directory.Extra as IO

--------------------------------------------------------------------------------
main :: IO ()
main = shakeArgs shakeOptions $ do
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
    let src = "en_dict-20121020.tar.gz"
    download fcitxDataUrl src "c44a5d7847925eea9e4d2d04748d442cd28dd9299a0b572ef7d91eac4f5a6ceb"
    cmd_ "tar" "xf" src out
  "en_dict.fscd" %> \out -> do
    let src = "en_dict.txt"
    need [src]
    compSpellDict <- getEnvWithDefault "/usr/lib/fcitx5/libexec/comp-spell-dict" "COMP_SPELL_DICT"
    cmd_ compSpellDict "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' "en_dict.fscd" $ "spell-dict" </> "en_dict.fscd"

--------------------------------------------------------------------------------
libimeRepoDataUrl :: String
libimeRepoDataUrl = "https://raw.githubusercontent.com/fcitx/libime/1.0.7/data/"

tableDictNames :: [String]
tableDictNames = ["db", "erbi", "qxm", "wanfeng", "wbpy", "wbx", "zrm", "cj"]

libimeRule :: Rules ()
libimeRule = do
  openGramApraRule
  openGramDictRule
  tableDictRule
  "libime" ~> do
    copyFile' "sc.dict" $ "libime" </> "data" </> "sc.dict"
    copyFile' "zh_CN.lm.predict" $ "libime" </> "data" </> "zh_CN.lm.predict"
    copyFile' "zh_CN.lm" $ "libime" </> "data" </> "zh_CN.lm"
    forM_ tableDictNames $ \table ->
      let name = table <.> "main.dict"
       in copyFile' name ("libime" </> "table" </> name)

openGramApraRule :: Rules ()
openGramApraRule = do
  "lm_sc.3gm.arpa" %> \out -> do
    let src = "lm_sc.3gm.arpa-20140820.tar.bz2"
    download fcitxDataUrl src "751bab7c55ea93a2cedfb0fbb7eb09f67d4da9c2c55496e5f31eb8580f1d1e2f"
    cmd_ "tar" "xf" src out
  "kenlm_sc.arpa" %> \out -> do
    let script = "convert_open_gram_arpa.py"
        src = "lm_sc.3gm.arpa"
    download libimeRepoDataUrl script "fb8a10aa083c7e566d099e9000539c5c7243c24a44c2357dc4e0f02461b6d011"
    need [src]
    (Stdout txt) <- cmd "python" script src
    writeFile' out txt
  "zh_CN.lm" %> \out -> do
    let src = "kenlm_sc.arpa"
    need [src]
    cmd_ "libime_slm_build_binary -s -a 22 -q 8 trie" src out
  "zh_CN.lm.predict" %> \out -> do
    let src1 = "zh_CN.lm"
        src2 = "kenlm_sc.arpa"
    need [src1, src2]
    cmd_ "libime_prediction" src1 src2 out

openGramDictRule :: Rules ()
openGramDictRule = do
  "dict.utf8" %> \out -> do
    let src = "dict.utf8-20211021.tar.xz"
    download fcitxDataUrl src "300597e6f7f79f788480fd665de8a07bfe90227048b5a7e39f40f43a62a981df"
    cmd_ "tar" "xf" src out
  "dict.converted" %> \out -> do
    let script = "convert_open_gram_dict.py"
        src = "dict.utf8"
    download libimeRepoDataUrl script "e8c42f4d4863dbf32eb7826097ad74b7bc00f660eab913f06e485fffbc4fb8c4"
    need [src]
    (Stdout txt) <- cmd "python" script src
    writeFile' out txt
  "sc.dict" %> \out -> do
    let src = "dict.converted"
    need [src]
    cmd_ "libime_pinyindict" src out

tableDictRule :: Rules ()
tableDictRule = do
  (<.> "txt") <$> tableDictNames |%> \out -> do
    let src = "table.tar.gz"
    download fcitxDataUrl src "6196053c724125e3ae3d8bd6b2f9172d0c83b65b0d410d3cde63b7a8d6ab87b7"
    (Stdout txt) <- cmd "tar" "xf" src out
    produces $ lines txt
  "*.main.dict" %> \out -> do
    let src = takeWhile (/= '.') out <.> "txt"
    need [src]
    cmd_ "libime_tabledict" src out

--------------------------------------------------------------------------------
chineseAddonsRepoDataUrl :: String
chineseAddonsRepoDataUrl = "https://raw.githubusercontent.com/fcitx/fcitx5-chinese-addons/5.0.9/im/pinyin/"

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
  let dict name sha256 =
        name <.> "dict" %> \out -> do
          let src = name <.> "txt"
          download chineseAddonsRepoDataUrl src sha256
          cmd_ "libime_pinyindict" src out

  dict "chaizi" "cd659605360120f7390fda1a278eea12c4df6d763a95c8099068ab09cfafd058"
  dict "emoji" "1e34d7ac78d1e1879829d1a8533242047ec8b5c2a2d64f26ef6a6bf2dc551994"

pinyinStrokeRule :: Rules ()
pinyinStrokeRule = do
  "py_stroke.mb" %> \out -> do
    let src = "py_stroke-20121124.tar.gz"
    download fcitxDataUrl src "8eb128a9bfa43952e67cf2fcee1fd134c6f4cfd317bc2f6c38a615f5eb64e248"
    cmd_ "tar" "xf" src out

pinyinTableRule :: Rules ()
pinyinTableRule = do
  "py_table.mb" %> \out -> do
    let src = "py_table-20121124.tar.gz"
    download fcitxDataUrl src "42146ac97de6c13d55f9e99ed873915f4c66739e9c11532a34556badf9792c04"
    cmd_ "tar" "xf" src out

--------------------------------------------------------------------------------

newtype Boost = Boost {boostLib :: String}
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <- addOracle $ \(WithAndroidEnv Boost {..} AndroidEnv {..}) -> do
    boostAndroidSrc <- getCanonicalizedRootSrc "Boost-for-Android"
    let boostVersion = "1.78.0"
        boostTar = "boost_" <> replace "." "_" boostVersion <.> "tar" <.> "bz2"
        boostUrl = "https://boostorg.jfrog.io/artifactory/main/release/" <> boostVersion <> "/source/"
    download boostUrl boostTar "8681f175d4bdb26c52222665793eef08490d7758529330f98d3b29dd0735bccc"
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
      ["//*.hpp"]
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
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Fmt = ()

fmtRule :: Rules ()
fmtRule = do
  buildFmt <- addOracleCache $ \(WithAndroidEnv Fmt env@AndroidEnv {..}) -> do
    fmtSrc <- getCanonicalizedRootSrc "fmt"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    (src, production) <- getSrcAndProduction "fmt"
    needSrc fmtSrc src
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "fmt" </> a
        produces $ fmap (outPrefix </>) production
        cmd_
          (Cwd fmtSrc)
          cmake
          "-B"
          "build"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_CXX_FLAGS=-std=c++17",
            "-DFMT_TEST=OFF",
            "-DFMT_DOC=OFF"
          ]
        cmd_ (Cwd fmtSrc) cmake "--build" "build"
        cmd_ (Cwd fmtSrc) cmake "--build" "build" "--target" "install"
  "fmt" ~> do
    env <- getAndroidEnv
    buildFmt $ WithAndroidEnv Fmt env

--------------------------------------------------------------------------------

data Libevent = Libevent
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Libevent = ()

libeventRule :: Rules ()
libeventRule = do
  buildLibevent <- addOracleCache $ \(WithAndroidEnv Libevent env@AndroidEnv {..}) -> do
    libeventSrc <- getCanonicalizedRootSrc "libevent"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    (src, production) <- getSrcAndProduction "libevent"
    needSrc libeventSrc src
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libevent" </> a
        produces $ fmap (outPrefix </>) production
        cmd_
          (Cwd libeventSrc)
          cmake
          "-B"
          "build"
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
        cmd_ (Cwd libeventSrc) cmake "--build" "build"
        cmd_ (Cwd libeventSrc) cmake "--build" "build" "--target" "install"
        -- post patch
        cmd_ (Cwd outPrefix) "sed" "-i" "121s/_event_h/true/" "lib/cmake/libevent/LibeventConfig.cmake"
        cmd_ (Cwd outPrefix) "sed" "-i" "135s/_event_lib/true/" "lib/cmake/libevent/LibeventConfig.cmake"
        cmd_ (Cwd outPrefix) "sed" "-i" ["45{/^set(_IMPORT_PREFIX/d}", "lib/cmake/libevent/LibeventTargets-static.cmake"]
        cmd_ (Cwd outPrefix) "sed" "-i" ["45iget_filename_component(LIBEVENT_CMAKE_DIR \"${CMAKE_CURRENT_LIST_FILE}\" PATH)", "lib/cmake/libevent/LibeventTargets-static.cmake"]
        cmd_ (Cwd outPrefix) "sed" "-i" ["46iget_filename_component(_IMPORT_PREFIX \"${LIBEVENT_CMAKE_DIR}/../../..\" ABSOLUTE)", "lib/cmake/libevent/LibeventTargets-static.cmake"]
  "libevent" ~> do
    env <- getAndroidEnv
    buildLibevent $ WithAndroidEnv Libevent env

--------------------------------------------------------------------------------

data LibintlLite = LibintlLite
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult LibintlLite = ()

libintlLiteRule :: Rules ()
libintlLiteRule = do
  buildLibintlLite <- addOracleCache $ \(WithAndroidEnv LibintlLite env@AndroidEnv {..}) -> do
    libintlSrc <- getCanonicalizedRootSrc "libintl-lite"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    (src, production) <- getSrcAndProduction "libintl-lite"
    needSrc libintlSrc src
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libintl-lite" </> a
        produces $ fmap (outPrefix </>) production
        cmd_
          (Cwd libintlSrc)
          cmake
          "-B"
          "build"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_BUILD_TYPE=Release"
          ]
        cmd_ (Cwd libintlSrc) cmake "--build" "build"
        cmd_ (Cwd libintlSrc) cmake "--build" "build" "--target" "install"
  "libintl-lite" ~> do
    env <- getAndroidEnv
    buildLibintlLite $ WithAndroidEnv LibintlLite env

--------------------------------------------------------------------------------

data Lua = Lua
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Lua = ()

luaRule :: Rules ()
luaRule = do
  buildLua <- addOracleCache $ \(WithAndroidEnv Lua env@AndroidEnv {..}) -> do
    luaSrc <- getCanonicalizedRootSrc "Lua"
    (src, production) <- getSrcAndProduction "lua"
    needSrc luaSrc src
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let toolchain = getCmakeToolchain env
    withAndroidEnv env $ \cmake abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "lua" </> a
        produces $ fmap (outPrefix </>) production
        cmd_
          (Cwd luaSrc)
          cmake
          "-B"
          "build"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DLUA_BUILD_BINARY=OFF",
            "-DLUA_BUILD_COMPILER=OFF"
          ]
        cmd_ (Cwd luaSrc) cmake "--build" "build"
        cmd_ (Cwd luaSrc) cmake "--build" "build" "--target" "install"
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
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

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
  sdkRoot <- fromMaybeM (fail "") $ getEnv "ANDROID_SDK_ROOT"
  ndkRoot <- env "ANDROID_NDK_ROOT"
  sdkCmakeVersion <- env "CMAKE_VERSION"
  platform <- read <$> env "ANDROID_PLATFORM"
  abi <- env "ABI"
  pure AndroidEnv {..}
  where
    env name = fromMaybeM (fail $ "Environment variable " <> name <> " is unset!") $ getEnv name

data WithAndroidEnv k = WithAndroidEnv k AndroidEnv
  deriving (Eq, Generic, Hashable, Binary, NFData)

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
  addOracleCache $ \MainPath -> takeDirectory <$> liftIO getMainPath

getMainPath :: HasCallStack => IO FilePath
getMainPath =
  withFrozenCallStack $
    canonicalizePath
      . srcLocFile
      . snd
      . fromJust
      . find ((== "getMainPath") . fst)
      . getCallStack
      $ callStack

--------------------------------------------------------------------------------

getSrcAndProduction :: String -> Action ([String], [String])
getSrcAndProduction name = do
  list <- getCanonicalizedRootSrc "list"
  (,) <$> readFileLines (list </> name <> "-i") <*> readFileLines (list </> name <> "-o")

needSrc :: FilePath -> [FilePattern] -> Action ()
needSrc srcRoot src =
  getDirectoryFiles
    srcRoot
    src
    >>= need . fmap (srcRoot </>)

--------------------------------------------------------------------------------

data DownloadFile = DownloadFile
  { downloadBaseUrl :: String,
    downloadFileName :: FilePath,
    downloadSha256 :: String
  }
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

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
