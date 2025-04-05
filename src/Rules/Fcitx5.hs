module Rules.Fcitx5
  ( hostFcitx5Rule,
    spellDictRule,
  )
where

import Base
import Data.Maybe (fromJust)

hostFcitx5Rule :: Rules ()
hostFcitx5Rule = do
  "host-fcitx5" ~> do
    let fcitx5Src = "fcitx5"
    cmd_
      "cmake"
      "-B"
      (fcitx5Src </> "build-host")
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> outputDir,
        "-DCMAKE_PREFIX_PATH=" <> outputDir,
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
      (fcitx5Src </> "build-host")
      "--target"
      [ "Fcitx5Utils",
        "comp-spell-dict"
      ]
    -- ignore install errors
    Exit _ <- cmd "cmake" "--install" (fcitx5Src </> "build-host")
    -- install "comp-spell-dict" manually
    copyFile' (fcitx5Src </> "build-host" </> "bin" </> "comp-spell-dict") $ outputDir </> "bin" </> "comp-spell-dict"
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
    let compSpellDict = outputDir </> "bin" </> "comp-spell-dict"
    let src = outputDir </> "en_dict.txt"
    need [src, "host-fcitx5"]
    cmd_ compSpellDict "--comp-dict" [src, out]
  "spell-dict" ~> do
    copyFile' (outputDir </> "en_dict.fscd") $ outputDir </> "spell-dict" </> "en_dict.fscd"
