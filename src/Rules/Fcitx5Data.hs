module Rules.Fcitx5Data (spellDictRule) where

import Base

spellDictRule :: Rules ()
spellDictRule = do
  outputDir </> "en_dict.txt" %> \out -> do
    src <- getConfig' "en_dict"
    sha256 <- getConfig' "en_dict_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "en_dict.fscd" %> \out -> do
    let src = outputDir </> "en_dict.txt"
    need [src]
    compSpellDict <- getEnvWithDefault "/usr/lib/fcitx5/libexec/comp-spell-dict" "COMP_SPELL_DICT"
    cmd_ compSpellDict "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' (outputDir </> "en_dict.fscd") $ outputDir </> "spell-dict" </> "en_dict.fscd"
