module Rules.Fcitx5
  ( hostFcitx5Rule,
    spellDictRule,
  )
where

import Base

hostFcitx5Rule :: Rules ()
hostFcitx5Rule = do
  "host-fcitx5" ~> do
    let fcitx5Src = "fcitx5"
    let buildDir = outputDir </> "fcitx5-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_
      "cmake"
      "-B"
      buildDir
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix,
        "-DENABLE_TEST=OFF",
        "-DENABLE_COVERAGE=OFF",
        "-DENABLE_ENCHANT=OFF",
        "-DENABLE_X11=OFF",
        "-DENABLE_WAYLAND=OFF",
        "-DENABLE_DBUS=OFF",
        "-DENABLE_DOC=OFF",
        "-DENABLE_SERVER=OFF",
        "-DENABLE_KEYBOARD=OFF",
        "-DENABLE_XDGAUTOSTART=OFF",
        "-DENABLE_EMOJI=OFF",
        "-DENABLE_LIBUUID=OFF"
      ]
      fcitx5Src
    cmd_
      "cmake"
      "--build"
      buildDir
      "--target"
      [ "Fcitx5Utils",
        "comp-spell-dict"
      ]
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" buildDir
    -- install "comp-spell-dict" manually
    copyFile' (buildDir </> "src" </> "modules" </> "spell" </> "comp-spell-dict") $ hostPrefix </> "bin" </> "comp-spell-dict"
    pure ()

--------------------------------------------------------------------------------

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
    need [src, "host-fcitx5"]
    execute "comp-spell-dict" "--comp-dict" src out
  "spell-dict" ~> do
    copyFile' (outputDir </> "en_dict.fscd") $ outputDir </> "spell-dict" </> "en_dict.fscd"
