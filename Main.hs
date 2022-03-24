{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (forM_, void)
import Control.Monad.Extra (fromMaybeM)
import Control.Monad.Trans.Maybe
import Data.List.Extra (find, split)
import Data.Maybe (fromJust)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics (Generic)
import GHC.Stack
import System.Directory.Extra

--------------------------------------------------------------------------------
main :: IO ()
main = shakeArgs shakeOptions $ do
  mainPathRule
  spellDict
  libime
  chineseAddons
  fmt
  libevent

fcitxDataUrl :: String
fcitxDataUrl = "https://download.fcitx-im.org/data/"

--------------------------------------------------------------------------------

spellDict :: Rules ()
spellDict = do
  "en_dict-20121020.tar.gz" %> \out -> do
    download fcitxDataUrl out "c44a5d7847925eea9e4d2d04748d442cd28dd9299a0b572ef7d91eac4f5a6ceb"
  "en_dict.txt" ~> do
    let src = "en_dict-20121020.tar.gz"
    need [src]
    cmd_ "tar" "xf" src
  "en_dict.fscd" %> \out -> do
    let src = "en_dict.txt"
    need [src]
    cmd_ "comp-spell-dict" "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' "en_dict.fscd" $ "spell-dict" </> "en_dict.fscd"

--------------------------------------------------------------------------------
libimeRepoDataUrl :: String
libimeRepoDataUrl = "https://raw.githubusercontent.com/fcitx/libime/1.0.7/data/"

libime :: Rules ()
libime = do
  openGramApra
  openGramDict
  tableDict
  "libime" ~> do
    copyFile' "sc.dict" $ "libime" </> "data" </> "sc.dict"
    copyFile' "zh_CN.lm.predict" $ "libime" </> "data" </> "zh_CN.lm.predict"
    copyFile' "zh_CN.lm" $ "libime" </> "data" </> "zh_CN.lm"
    forM_ ["db", "erbi", "qxm", "wanfeng", "wbpy", "wbx", "zrm", "cj"] $ \table ->
      let name = table <.> "main.dict"
       in copyFile' name ("libime" </> "table" </> name)

openGramApra :: Rules ()
openGramApra = do
  "convert_open_gram_arpa.py" %> \out ->
    download libimeRepoDataUrl out "fb8a10aa083c7e566d099e9000539c5c7243c24a44c2357dc4e0f02461b6d011"
  "lm_sc.3gm.arpa-20140820.tar.bz2" %> \out ->
    download fcitxDataUrl out "751bab7c55ea93a2cedfb0fbb7eb09f67d4da9c2c55496e5f31eb8580f1d1e2f"
  "lm_sc.3gm.arpa" ~> do
    let src = "lm_sc.3gm.arpa-20140820.tar.bz2"
    need [src]
    cmd_ "tar" "xf" src
  "kenlm_sc.arpa" %> \out -> do
    let script = "convert_open_gram_arpa.py"
        src = "lm_sc.3gm.arpa"
    need [script, src]
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

openGramDict :: Rules ()
openGramDict = do
  "convert_open_gram_dict.py" %> \out ->
    download libimeRepoDataUrl out "e8c42f4d4863dbf32eb7826097ad74b7bc00f660eab913f06e485fffbc4fb8c4"
  "dict.utf8-20211021.tar.xz" %> \out ->
    download fcitxDataUrl out "300597e6f7f79f788480fd665de8a07bfe90227048b5a7e39f40f43a62a981df"
  "dict.utf8" ~> do
    let src = "dict.utf8-20211021.tar.xz"
    need [src]
    cmd_ "tar" "xf" src
  "dict.converted" %> \out -> do
    let script = "convert_open_gram_dict.py"
        src = "dict.utf8"
    need [script, src]
    (Stdout txt) <- cmd "python" script src
    writeFile' out txt
  "sc.dict" %> \out -> do
    let src = "dict.converted"
    cmd_ "libime_pinyindict" src out

tableDict :: Rules ()
tableDict = do
  "table.tar.gz" %> \out ->
    download fcitxDataUrl out "6196053c724125e3ae3d8bd6b2f9172d0c83b65b0d410d3cde63b7a8d6ab87b7"
  "table_txt" ~> do
    let src = "table.tar.gz"
    need [src]
    (Stdout txt) <- cmd "tar" "xfv" src
    produces $ lines txt
  "*.main.dict" %> \out -> do
    let src = takeWhile (/= '.') out
    need ["table_txt"]
    cmd_ "libime_tabledict" src out

--------------------------------------------------------------------------------
chineseAddonsRepoDataUrl :: String
chineseAddonsRepoDataUrl = "https://raw.githubusercontent.com/fcitx/fcitx5-chinese-addons/5.0.9/im/pinyin/"

chineseAddons :: Rules ()
chineseAddons = do
  pinyinDict
  pinyinStroke
  pinyinTable
  "chinese-addons-data" ~> do
    copyFile' "emoji.dict" $ "chinese-addons-data" </> "pinyin" </> "emoji.dict"
    copyFile' "chaizi.dict" $ "chinese-addons-data" </> "pinyin" </> "chaizi.dict"
    copyFile' "py_table.mb" $ "chinese-addons-data" </> "pinyinhelper" </> "py_table.mb"
    copyFile' "py_stroke.mb" $ "chinese-addons-data" </> "pinyinhelper" </> "py_stroke.mb"

pinyinDict :: Rules ()
pinyinDict = do
  let txt name sha256 =
        name <.> "txt" %> \out ->
          download chineseAddonsRepoDataUrl out sha256
      dict name =
        name <.> "dict" %> \out -> do
          let src = name <.> "txt"
          need [src]
          cmd_ "libime_pinyindict" src out

  txt "chaizi" "cd659605360120f7390fda1a278eea12c4df6d763a95c8099068ab09cfafd058"
  dict "chaizi"

  txt "emoji" "1e34d7ac78d1e1879829d1a8533242047ec8b5c2a2d64f26ef6a6bf2dc551994"
  dict "emoji"

pinyinStroke :: Rules ()
pinyinStroke = do
  "py_stroke-20121124.tar.gz" %> \out ->
    download fcitxDataUrl out "8eb128a9bfa43952e67cf2fcee1fd134c6f4cfd317bc2f6c38a615f5eb64e248"
  "py_stroke.mb" ~> do
    let src = "py_stroke-20121124.tar.gz"
    need [src]
    cmd_ "tar" "xf" src

pinyinTable :: Rules ()
pinyinTable = do
  "py_table-20121124.tar.gz" %> \out ->
    download fcitxDataUrl out "42146ac97de6c13d55f9e99ed873915f4c66739e9c11532a34556badf9792c04"
  "py_table.mb" ~> do
    let src = "py_table-20121124.tar.gz"
    need [src]
    cmd_ "tar" "xf" src

--------------------------------------------------------------------------------

data Boost = Boost
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Boost = ()

--------------------------------------------------------------------------------

data Fmt = Fmt
  deriving (Show, Typeable, Eq, Generic, Hashable, Binary, NFData)

type instance RuleResult Fmt = ()

fmt :: Rules ()
fmt = do
  buildFmt <- addOracle $ \(WithAndroidEnv Fmt env@AndroidEnv {..}) -> do
    fmtSrc <- getCanonicalizedRootSrc "fmt"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let abiList = getABIList env
        cmake = getSdkCmake env
    forM_ abiList $ \a -> do
      cmd_
        (Cwd fmtSrc)
        cmake
        "-B"
        "build"
        [ "-DCMAKE_TOOLCHAIN_FILE=" <> ndkRoot </> "build" </> "cmake" </> "android.toolchain.cmake",
          "-DANDROID_ABI=" <> a,
          "-DANDROID_PLATFORM=" <> show platform,
          "-DANDROID_STL=c++_shared",
          "-DCMAKE_INSTALL_PREFIX=" <> out </> "fmt" </> a,
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

libevent :: Rules ()
libevent = do
  buildLibevent <- addOracle $ \(WithAndroidEnv Libevent env@AndroidEnv {..}) -> do
    libeventSrc <- getCanonicalizedRootSrc "libevent"
    out <- liftIO $ getCurrentDirectory >>= canonicalizePath
    let abiList = getABIList env
        cmake = getSdkCmake env
    forM_ abiList $ \a -> do
      let outPrefix = out </> "libevent" </> a
      cmd_
        (Cwd libeventSrc)
        cmake
        "-B"
        "build"
        [ "-DCMAKE_TOOLCHAIN_FILE=" <> ndkRoot </> "build" </> "cmake" </> "android.toolchain.cmake",
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
      cmd_ (Cwd outPrefix) "sed" "-i" "45{/^set(_IMPORT_PREFIX/d}" "lib/cmake/libevent/LibeventTargets-static.cmake"
      cmd_ (Cwd outPrefix) "sed" "-i" "45iget_filename_component(LIBEVENT_CMAKE_DIR \"${CMAKE_CURRENT_LIST_FILE}\" PATH)" "lib/cmake/libevent/LibeventTargets-static.cmake"
      cmd_ (Cwd outPrefix) "sed" "-i" "46iget_filename_component(_IMPORT_PREFIX \"${LIBEVENT_CMAKE_DIR}/../../..\" ABSOLUTE)" "lib/cmake/libevent/LibeventTargets-static.cmake"
  "libevent" ~> do
    env <- getAndroidEnv
    buildLibevent $ WithAndroidEnv Libevent env

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

getAndroidEnv :: Action AndroidEnv
getAndroidEnv = fromMaybeM
  ( fail "Can not construct AndroidEnv. Please make sure that the following environment variables are set: ANDROID_SDK_ROOT, ANDROID_NDK_ROOT, ANDROID_SDK_CMAKE_VERSION, ANDROID_PLATFORM, and ANDROID_ABI."
  )
  $ runMaybeT $ do
    sdkRoot <- MaybeT $ getEnv "ANDROID_SDK_ROOT"
    ndkRoot <- MaybeT $ getEnv "ANDROID_NDK_ROOT"
    sdkCmakeVersion <- MaybeT $ getEnv "ANDROID_SDK_CMAKE_VERSION"
    platform <- fmap read $ MaybeT $ getEnv "ANDROID_PLATFORM"
    abi <- MaybeT $ getEnv "ANDROID_ABI"
    pure AndroidEnv {..}

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
  addOracle $ \MainPath -> takeDirectory <$> liftIO getMainPath

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

download :: String -> FilePath -> String -> Action ()
download baseUrl fileName sha256 = do
  let url = baseUrl <> fileName
  cmd_ "curl" "-LO" url
  if null sha256
    then do
      (Stdout result) <- cmd "sha256sum" fileName
      fail $ "You entered an empty sha256 for " <> fileName <> ". The output of sha256sum is " <> result
    else cmd_ (Stdin $ sha256 <> " " <> fileName) "sha256sum" "--check --status"

--------------------------------------------------------------------------------
