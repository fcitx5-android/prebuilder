{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibChewing where

import Base
import CMakeBuilder
import Control.Arrow ((>>>))

data LibChewing = LibChewing
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibChewing = ()

libchewingRule :: Rules ()
libchewingRule = do
  let libchewingSrc = "libchewing"
      dictOutputDir = outputDir </> "chewing-dict"
      dictSrcDir = libchewingSrc </> "data"

  buildLibchewing <-
    useCMake $  (cmakeBuilder "libchewing")
        { cmakeFlags = const ["-DWITH_SQLITE3=OFF"],
          preBuild = \_ src -> do
            -- CMakeLists is changed in last build
            cmd_ (Cwd src) Shell "git checkout -- CMakeLists.txt"
            -- skip data and shared lib
            -- merge libuserphrase.a into libchewing.a
            -- remove absolute path by CHEWING_DATADIR macro
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd src) "git apply ../patches/libchewing.patch",
          postBuildEachABI = stripLib "lib/libchewing.a" >>> removePkgConfig
        }
  phony "generateDict" $ do
    cmd_ (Cwd libchewingSrc) "./autogen.sh"
    cmd_ (Cwd libchewingSrc) "./configure --with-sqlite3=no"
    cmd_ (Cwd libchewingSrc) "make"

  libchewingSrc </> "data/*" %> \_ -> need ["generateDict"]

  "chewing-dict" ~> do
    copyFile' (dictSrcDir </> "dictionary.dat") (dictOutputDir </> "dictionary.dat")
    copyFile' (dictSrcDir </> "index_tree.dat") (dictOutputDir </> "index_tree.dat")
    copyFile' (dictSrcDir </> "pinyin.tab") (dictOutputDir </> "pinyin.tab")
    copyFile' (dictSrcDir </> "swkb.dat") (dictOutputDir </> "swkb.dat")
    copyFile' (dictSrcDir </> "symbols.dat") (dictOutputDir </> "symbols.dat")

  "libchewing" ~> do
    need ["chewing-dict"]
    buildWithAndroidEnv buildLibchewing LibChewing
