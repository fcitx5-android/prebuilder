module Rules.AnthyData (anthyDictRule) where

import Base

anthyDictRule :: Rules ()
anthyDictRule = do
  outputDir </> "anthy.dic" %> \out -> do
    let anthySrc = "anthy-unicode"
    cmd_ (Cwd anthySrc) "./autogen.sh"
    cmd_ (Cwd anthySrc) "make"
    copyFile' (anthySrc </> "mkanthydic" </> "anthy.dic") out
  "anthy-dict" ~> do
    copyFile' (outputDir </> "anthy.dic") (outputDir </> "anthy-dict" </> "anthy.dic")
