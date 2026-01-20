module Rules.Fcitx5
  ( hostFmtRule,
    hostFcitx5Rule,
    spellDictRule,
  )
where

import Base

hostFmtRule :: Rules ()
hostFmtRule = do
  "host-fmt" ~> do
    let fmtSrc = "fmt"
    let buildDir = outputDir </> "fmt-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_
      "cmake"
      "-B"
      buildDir
      "-G"
      "Ninja"
      [ "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix,
        "-DFMT_TEST=OFF" ,
        "-DFMT_DOC=OFF"
      ]
      fmtSrc
    cmd_ "cmake" "--build" buildDir
    cmd_ "cmake" "--install" buildDir


hostFcitx5Rule :: Rules ()
hostFcitx5Rule = do
  "host-fcitx5" ~> do
    need ["host-fmt"]
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
        "-DCMAKE_FIND_ROOT_PATH=" <> hostPrefix,  -- for find_package
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
