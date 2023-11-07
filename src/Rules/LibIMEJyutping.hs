module Rules.LibIMEJyutping (libIMEJyutpingRule) where

import Base

libIMEJyutpingRule :: Rules ()
libIMEJyutpingRule = do
  jyutpingToolsRule
  jyutpingDictRule
  jyutpingLmRule
  "libime-jyutping" ~> do
    copyFile' (outputDir </> "jyutping.dict") $ outputDir </> "libime-jyutping" </> "jyutping.dict"
    copyFile' (outputDir </> "zh_HK.lm") $ outputDir </> "libime-jyutping" </> "zh_HK.lm"
    copyFile' (outputDir </> "zh_HK.lm.predict") $ outputDir </> "libime-jyutping" </> "zh_HK.lm.predict"

jyutpingToolsRule :: Rules ()
jyutpingToolsRule = do
  "libime-jyutping-tools" ~> do
    need ["libime-tools", "host-libzstd"]
    let libIMEJyutpingSrc = "libime-jyutping"
    cmd_
      "cmake"
      "-B"
      (libIMEJyutpingSrc </> "build")
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> outputDir,
        "-DCMAKE_PREFIX_PATH=" <> outputDir,
        "-DENABLE_TEST=OFF",
        "-DENABLE_ENGINE=OFF"
      ]
      libIMEJyutpingSrc
    cmd_ "cmake" "--build" (libIMEJyutpingSrc </> "build") "--target" "libime_jyutpingdict"
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" (libIMEJyutpingSrc </> "build") "--component" "lib"
    Exit _ <- cmd "cmake" "--install" (libIMEJyutpingSrc </> "build") "--component" "tools"
    pure ()

jyutpingDictRule :: Rules ()
jyutpingDictRule = do
  outputDir </> "words.txt" %> \out -> do
    src <- getConfig' "jyutping_dict"
    sha256 <- getConfig' "jyutping_dict_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "jyutping.dict" %> \out -> do
    let src = outputDir </> "words.txt"
    need ["libime-jyutping-tools", src]
    execute "libime_jyutpingdict" src out

jyutpingLmRule :: Rules ()
jyutpingLmRule = do
  outputDir </> "hk.arpa" %> \out -> do
    src <- getConfig' "jyutping_model"
    sha256 <- getConfig' "jyutping_model_sha256"
    fcitxDataUrl <- getConfig' "fcitx_data_url"
    tar <- download fcitxDataUrl src sha256
    cmd_ "tar" "xf" tar "-C" outputDir (takeFileName out)
  outputDir </> "zh_HK.lm" %> \out -> do
    let src = outputDir </> "hk.arpa"
    need ["libime-tools", src]
    execute "libime_slm_build_binary" "-s -a 22 -q 8 trie" src out
  outputDir </> "zh_HK.lm.predict" %> \out -> do
    let src1 = outputDir </> "zh_HK.lm"
        src2 = outputDir </> "hk.arpa"
    need ["libime-tools", src1, src2]
    execute "libime_prediction" src1 src2 out
