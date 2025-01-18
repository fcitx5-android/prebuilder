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
            -- CMakeLists is changed in last build
            cmd_ (Cwd src) Shell "git checkout ."
            -- skip data and shared lib
            -- merge libuserphrase.a into libchewing.a
            -- remove absolute path by CHEWING_DATADIR macro
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd src) "git apply ../patches/libchewing.patch",
            cmakeFlags =
              const 
                [ "-DBUILD_SHARED_LIBS=OFF",
                  "-DBUILD_TESTING=OFF",
                  "-DWITH_SQLITE3=OFF",
                  "-DWITH_RUST=OFF"
                ]
        }

  "chewing-dict" ~> do
    let libchewingBuildHost = outputDir </> "libchewing-build-host"
        dictSrcDir = libchewingBuildHost </> "data"
    cmd_ (Cwd libchewingSrc) Shell "git checkout ."
    cmd_
      "cmake"
      "-B"
      libchewingBuildHost
      "-G"
      "Ninja"
      [ "-DBUILD_SHARED_LIBS=OFF",
        "-DBUILD_TESTING=OFF",
        "-DWITH_SQLITE3=OFF",
        "-DWITH_RUST=OFF"
      ]
      libchewingSrc
    cmd_
      "cmake"
      "--build"
      libchewingBuildHost
      "--target"
      [ "data",
        "all_static_data"
      ]
    copyFile' (dictSrcDir </> "dictionary.dat") (dictOutputDir </> "dictionary.dat")
    copyFile' (dictSrcDir </> "index_tree.dat") (dictOutputDir </> "index_tree.dat")
    copyFile' (dictSrcDir </> "pinyin.tab") (dictOutputDir </> "pinyin.tab")
    copyFile' (dictSrcDir </> "swkb.dat") (dictOutputDir </> "swkb.dat")
    copyFile' (dictSrcDir </> "symbols.dat") (dictOutputDir </> "symbols.dat")

  "libchewing" ~> do
    need ["chewing-dict"]
    buildWithAndroidEnv buildLibchewing LibChewing
