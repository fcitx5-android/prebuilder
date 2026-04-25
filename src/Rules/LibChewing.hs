{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibChewing where

import Base
import CMakeBuilder

data LibChewing = LibChewing
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibChewing = ()

libchewingRule :: Rules ()
libchewingRule = do
  let libchewingSrc = "libchewing"
      dictOutputDir = outputDir </> "chewing-dict"

  buildLibchewing <-
    useCMake $
      (cmakeBuilder "libchewing")
        { preBuild = BuildAction $ \_ src -> do
            -- install rust toolchain
            cmd_ Shell "rustup toolchain install $RUST_VERSION"
            -- CMakeLists.txt changed in last build
            cmd_ (Cwd src) Shell "git checkout ."
            -- disable data/doc, remove absolute path, optimize library size
            cmd_ (Cwd src) "git apply ../patches/libchewing.patch",
          preBuildEachABI = BuildActionABI $ \_ env -> do
            -- install rust target for this abi
            let targetName = case (buildEnvABI env) of
                  "armeabi-v7a" -> "armv7-linux-androideabi"
                  "arm64-v8a"   -> "aarch64-linux-android"
                  "x86"         -> "i686-linux-android"
                  "x86_64"      -> "x86_64-linux-android"
                  _             -> fail "Unknown Android ABI"
            cmd_ "rustup" "target" "add" targetName,
          cmakeFlags =
            const 
              [ "-DBUILD_SHARED_LIBS=OFF",
                "-DBUILD_TESTING=OFF",
                "-DBUILD_DATA=OFF",
                "-DBUILD_DOC=OFF",
                "-DWITH_SQLITE3=OFF"
              ]
        }

  "chewing-dict" ~> do
    -- install rust
    cmd_ Shell "rustup toolchain install $RUST_VERSION"
    let libchewingBuildHost = outputDir </> "libchewing-build-host"
        dictSrcDir = libchewingBuildHost </> "data"
    cmd_ (Cwd libchewingSrc) Shell "git checkout ."
    cmd_
      "cmake"
      "-B"
      libchewingBuildHost
      "-G"
      "Ninja"
      [ "-DBUILD_TESTING=OFF",
        "-DWITH_SQLITE3=OFF",
        "-DBUILD_DOC=OFF"
      ]
      libchewingSrc
    cmd_
      "cmake"
      "--build"
      libchewingBuildHost
      "--target"
      [ "dict_chewing",
        "misc"
      ]
    copyFile' (dictSrcDir </> "dict" </> "chewing" </> "tsi.dat") (dictOutputDir </> "tsi.dat")
    copyFile' (dictSrcDir </> "dict" </> "chewing" </> "word.dat") (dictOutputDir </> "word.dat")
    copyFile' (dictSrcDir </> "misc" </> "swkb.dat") (dictOutputDir </> "swkb.dat")
    copyFile' (dictSrcDir </> "misc" </> "symbols.dat") (dictOutputDir </> "symbols.dat")

  "libchewing" ~> do
    need ["chewing-dict"]
    buildWithAndroidEnv buildLibchewing LibChewing
