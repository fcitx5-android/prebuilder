module Rules.Fcitx5Data
  ( spellDictRule,
    libimeRule,
    chineseAddonsRule,
  )
where

import Base
import Data.Maybe (fromJust)

fcitxDataUrl :: String
fcitxDataUrl = "https://download.fcitx-im.org/data/"

spellDictRule :: Rules ()
spellDictRule = do
  outputDir </> "en_dict.txt" %> \out -> do
    src <- getConfig' "en_dict"
    sha256 <- getConfig' "en_dict_sha256"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "en_dict.fscd" %> \out -> do
    let src = outputDir </> "en_dict.txt"
    need [src]
    compSpellDict <- getEnvWithDefault "/usr/lib/fcitx5/libexec/comp-spell-dict" "COMP_SPELL_DICT"
    cmd_ compSpellDict "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' (outputDir </> "en_dict.fscd") $ outputDir </> "spell-dict" </> "en_dict.fscd"

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
    copyFile' (outputDir </> "sc.dict") $ outputDir </> "libime" </> "data" </> "sc.dict"
    copyFile' (outputDir </> "extb.dict") $ outputDir </> "libime" </> "data" </> "extb.dict"
    copyFile' (outputDir </> "sc.lm") $ outputDir </> "libime" </> "data" </> "zh_CN.lm"
    copyFile' (outputDir </> "sc.lm.predict") $ outputDir </> "libime" </> "data" </> "zh_CN.lm.predict"
    forM_ tableDictNames $ \table ->
      let name = table <.> "main.dict"
       in copyFile' (outputDir </> name) (outputDir </> "libime" </> "table" </> name)

lmRule :: Rules ()
lmRule = do
  outputDir </> "lm_sc.arpa" %> \out -> do
    src <- getConfig' "lm_sc"
    sha256 <- getConfig' "lm_sc_sha256"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "sc.lm" %> \out -> do
    let src = outputDir </> "lm_sc.arpa"
    need [src]
    cmd_ "libime_slm_build_binary -s -a 22 -q 8 trie" src out
  outputDir </> "sc.lm.predict" %> \out -> do
    let src1 = outputDir </> "sc.lm"
        src2 = outputDir </> "lm_sc.arpa"
    need [src1, src2]
    cmd_ "libime_prediction" src1 src2 out

dictRule :: Rules ()
dictRule = do
  (outputDir </>) . ("dict_" <>) . (<.> "txt") <$> dictNames |%> \out -> do
    src <- getConfig' "dict"
    sha256 <- getConfig' "dict_sha256"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "dict") <$> dictNames |%> \out -> do
    let src = outputDir </> "dict_" <> takeBaseName out <.> "txt"
    need [src]
    cmd_ "libime_pinyindict" src out

tableDictRule :: Rules ()
tableDictRule = do
  (outputDir </>) . (<.> "txt") <$> tableDictNames |%> \out -> do
    src <- getConfig' "table"
    sha256 <- getConfig' "table_sha256"
    tar <- download fcitxDataUrl src sha256
    (Stdout txt) <- cmd "tar" "xf" tar "-C" outputDir (takeFileName out)
    produces $ lines txt
  (outputDir </>) . (<.> "main.dict") <$> tableDictNames |%> \out -> do
    let src = outputDir </> fromJust (stripExtension "main.dict" $ takeFileName out) <.> "txt"
    need [src]
    cmd_ "libime_tabledict" src out

--------------------------------------------------------------------------------

chineseAddonsRule :: Rules ()
chineseAddonsRule = do
  pinyinDictRule
  pinyinStrokeRule
  pinyinTableRule
  "chinese-addons-data" ~> do
    copyFile' (outputDir </> "emoji.dict") $ outputDir </> "chinese-addons-data" </> "pinyin" </> "emoji.dict"
    copyFile' (outputDir </> "chaizi.dict") $ outputDir </> "chinese-addons-data" </> "pinyin" </> "chaizi.dict"
    copyFile' (outputDir </> "py_table.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_table.mb"
    copyFile' (outputDir </> "py_stroke.mb") $ outputDir </> "chinese-addons-data" </> "pinyinhelper" </> "py_stroke.mb"

pinyinDictRule :: Rules ()
pinyinDictRule = do
  let dict name =
        outputDir </> name <.> "dict" %> \out -> do
          let src = name <.> "txt"
          chineseAddonsRepoDataUrl <- getConfig' "chinese_addon_repo"
          sha256 <- getConfig' $ name <> "_sha256"
          txt <- download chineseAddonsRepoDataUrl src sha256
          cmd_ "libime_pinyindict" txt out
  dict "chaizi"
  dict "emoji"

pinyinStrokeRule :: Rules ()
pinyinStrokeRule = do
  outputDir </> "py_stroke.mb" %> \out -> do
    src <- getConfig' "py_stroke"
    sha256 <- getConfig' "py_stroke_sha256"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)

pinyinTableRule :: Rules ()
pinyinTableRule = do
  outputDir </> "py_table.mb" %> \out -> do
    src <- getConfig' "py_table"
    sha256 <- getConfig' "py_table_sha256"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
