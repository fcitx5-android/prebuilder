module Rules.LibIME
  ( libIMERule,
    chineseAddonsRule,
  )
where

import Base
import Data.Maybe (fromJust)

dictNames :: [String]
dictNames = ["sc", "extb"]

tableDictNames :: [String]
tableDictNames = ["db", "wbpy", "wbx", "zrm"]

libIMERule :: Rules ()
libIMERule = do
  libIMEToolsRule
  lmRule
  pinyinDictRule
  tableDictRule
  "libime" ~> do
    copyFile' (outputDir </> "sc.dict") $ outputDir </> "libime" </> "data" </> "sc.dict"
    copyFile' (outputDir </> "extb.dict") $ outputDir </> "libime" </> "data" </> "extb.dict"
    copyFile' (outputDir </> "sc.lm") $ outputDir </> "libime" </> "data" </> "zh_CN.lm"
    copyFile' (outputDir </> "sc.lm.predict") $ outputDir </> "libime" </> "data" </> "zh_CN.lm.predict"
    forM_ tableDictNames $ \table ->
      let name = table <.> "main.dict"
       in copyFile' (outputDir </> name) (outputDir </> "libime" </> "table" </> name)

--------------------------------------------------------------------------------

libIMEToolsRule :: Rules ()
libIMEToolsRule = do
  "libime-tools" ~> do
    need ["host-libzstd"]
    let libIMESrc = "libime"
    cmd_
      "cmake"
      "-B"
      (libIMESrc </> "build")
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> outputDir,
        "-DCMAKE_PREFIX_PATH=" <> outputDir,
        "-DENABLE_TEST=OFF"
      ]
      libIMESrc
    cmd_
      "cmake"
      "--build"
      (libIMESrc </> "build")
      "--target"
      [ "libime_slm_build_binary",
        "libime_prediction",
        "libime_pinyindict",
        "libime_history",
        "libime_tabledict"
      ]
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" (libIMESrc </> "build") "--component" "lib"
    Exit _ <- cmd "cmake" "--install" (libIMESrc </> "build") "--component" "header"
    Exit _ <- cmd "cmake" "--install" (libIMESrc </> "build") "--component" "tools"
    Exit _ <- cmd "cmake" "--install" (libIMESrc </> "build") "--component" "Devel"
    pure ()

--------------------------------------------------------------------------------

lmRule :: Rules ()
lmRule = do
  outputDir </> "lm_sc.arpa" %> \out -> do
    src <- getConfig' "lm_sc"
    sha256 <- getConfig' "lm_sc_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "sc.lm" %> \out -> do
    let src = outputDir </> "lm_sc.arpa"
    need ["libime-tools", src]
    execute "libime_slm_build_binary" "-s -a 22 -q 8 trie" src out
  outputDir </> "sc.lm.predict" %> \out -> do
    let src1 = outputDir </> "sc.lm"
        src2 = outputDir </> "lm_sc.arpa"
    need ["libime-tools", src1, src2]
    execute "libime_prediction" src1 src2 out

--------------------------------------------------------------------------------

pinyinDictRule :: Rules ()
pinyinDictRule = do
  (outputDir </>) . ("dict_" <>) . (<.> "txt") <$> dictNames |%> \out -> do
    src <- getConfig' "pinyin_dict"
    sha256 <- getConfig' "pinyin_dict_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "dict") <$> dictNames |%> \out -> do
    let src = outputDir </> "dict_" <> takeBaseName out <.> "txt"
    need ["libime-tools", src]
    execute "libime_pinyindict" src out

tableDictRule :: Rules ()
tableDictRule = do
  (outputDir </>) . (<.> "txt") <$> tableDictNames |%> \out -> do
    src <- getConfig' "table"
    sha256 <- getConfig' "table_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "main.dict") <$> tableDictNames |%> \out -> do
    let src = outputDir </> fromJust (stripExtension "main.dict" $ takeFileName out) <.> "txt"
    need ["libime-tools", src]
    execute "libime_tabledict" src out

--------------------------------------------------------------------------------

chineseAddonsRule :: Rules ()
chineseAddonsRule = do
  pinyinExtraDictRule
  pinyinStrokeRule
  pinyinTableRule
  "chinese-addons-data" ~> do
    copyFile' (outputDir </> "chaizi.dict") $ outputDir </> "chinese-addons-data" </> "pinyin" </> "chaizi.dict"
    copyFile' (outputDir </> "py_table.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_table.mb"
    copyFile' (outputDir </> "py_stroke.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_stroke.mb"

pinyinExtraDictRule :: Rules ()
pinyinExtraDictRule = do
  let dict name =
        outputDir </> name <.> "dict" %> \out -> do
          need ["libime-tools"]
          let src = name <.> "txt"
          chineseAddonsRepoDataUrl <- getConfig' "chinese_addon_repo"
          sha256 <- getConfig' $ name <> "_sha256"
          txt <- download chineseAddonsRepoDataUrl src sha256
          execute "libime_pinyindict" txt out
  dict "chaizi"

pinyinStrokeRule :: Rules ()
pinyinStrokeRule = do
  outputDir </> "py_stroke.mb" %> \out -> do
    src <- getConfig' "py_stroke"
    sha256 <- getConfig' "py_stroke_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)

pinyinTableRule :: Rules ()
pinyinTableRule = do
  outputDir </> "py_table.mb" %> \out -> do
    src <- getConfig' "py_table"
    sha256 <- getConfig' "py_table_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
