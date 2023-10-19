module Rules.ChewingData (chewingDictRule) where

import Base

chewingDictRule :: Rules ()
chewingDictRule = do
  let chewingSrc = "libchewing"
      dataDir = outputDir </> "chewing-dict"
  chewingSrc </> "data" </> "dictionary.dat" ~> do
    cmd_ (Cwd chewingSrc) "git checkout -- CMakeLists.txt"
    cmd_ (Cwd chewingSrc) "./autogen.sh"
    cmd_ (Cwd chewingSrc) "./configure --with-sqlite3=no"
    cmd_ (Cwd chewingSrc) "make"
  "chewing-dict" ~> do
    copyFile' (chewingSrc </> "data" </> "dictionary.dat") (dataDir </> "dictionary.dat")
    copyFile' (chewingSrc </> "data" </> "index_tree.dat") (dataDir </> "index_tree.dat")
    copyFile' (chewingSrc </> "data" </> "pinyin.tab") (dataDir </> "pinyin.tab")
    copyFile' (chewingSrc </> "data" </> "swkb.dat") (dataDir </> "swkb.dat")
    copyFile' (chewingSrc </> "data" </> "symbols.dat") (dataDir </> "symbols.dat")
