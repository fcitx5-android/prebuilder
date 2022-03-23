{-# OPTIONS_GHC -Wall #-}

module Main where

import Development.Shake

--------------------------------------------------------------------------------
main :: IO ()
main = shakeArgs shakeOptions $ do
  spellDict
  libime

fcitxDataUrl :: String
fcitxDataUrl = "https://download.fcitx-im.org/data/"

--------------------------------------------------------------------------------

spellDict :: Rules ()
spellDict = do
  "en_dict-20121020.tar.gz" %> \out -> do
    download fcitxDataUrl out "c44a5d7847925eea9e4d2d04748d442cd28dd9299a0b572ef7d91eac4f5a6ceb"
  "en_dict.txt" ~> do
    let src = "en_dict-20121020.tar.gz"
    need [src]
    cmd_ "tar" "xf" src
  "en_dict.fscd" %> \out -> do
    let src = "en_dict.txt"
    need [src]
    cmd_ "comp-spell-dict" "--comp-dict" [src, out]

--------------------------------------------------------------------------------
libimeRepoDataUrl :: String
libimeRepoDataUrl = "https://raw.githubusercontent.com/fcitx/libime/1.0.7/data/"

libime :: Rules ()
libime = do
  openGramApra
  openGramDict
  tableDict

openGramApra :: Rules ()
openGramApra = do
  "convert_open_gram_arpa.py" %> \out ->
    download libimeRepoDataUrl out "fb8a10aa083c7e566d099e9000539c5c7243c24a44c2357dc4e0f02461b6d011"
  "lm_sc.3gm.arpa-20140820.tar.bz2" %> \out ->
    download fcitxDataUrl out "751bab7c55ea93a2cedfb0fbb7eb09f67d4da9c2c55496e5f31eb8580f1d1e2f"
  "lm_sc.3gm.arpa" ~> do
    let src = "lm_sc.3gm.arpa-20140820.tar.bz2"
    need [src]
    cmd_ "tar" "xf" src
  "kenlm_sc.arpa" %> \out -> do
    let script = "convert_open_gram_arpa.py"
        src = "lm_sc.3gm.arpa"
    need [script, src]
    (Stdout txt) <- cmd "python" script src
    writeFile' out txt
  "zh_CN.lm" %> \out -> do
    let src = "kenlm_sc.arpa"
    need [src]
    cmd_ "libime_slm_build_binary -s -a 22 -q 8 trie" src out
  "zh_CN.lm.predict" %> \out -> do
    let src1 = "zh_CN.lm"
        src2 = "kenlm_sc.arpa"
    need [src1, src2]
    cmd_ "libime_prediction" src1 src2 out

openGramDict :: Rules ()
openGramDict = do
  "convert_open_gram_dict.py" %> \out ->
    download libimeRepoDataUrl out "e8c42f4d4863dbf32eb7826097ad74b7bc00f660eab913f06e485fffbc4fb8c4"
  "dict.utf8-20211021.tar.xz" %> \out ->
    download fcitxDataUrl out "300597e6f7f79f788480fd665de8a07bfe90227048b5a7e39f40f43a62a981df"
  "dict.utf8" ~> do
    let src = "dict.utf8-20211021.tar.xz"
    need [src]
    cmd_ "tar" "xf" src
  "dict.converted" %> \out -> do
    let script = "convert_open_gram_dict.py"
        src = "dict.utf8"
    need [script, src]
    (Stdout txt) <- cmd "python" script src
    writeFile' out txt
  "sc.dic" %> \out -> do
    let src = "dict.converted"
    cmd_ "libime_pinyindict" src out

tableDict :: Rules ()
tableDict = do
  "table.tar.gz" %> \out ->
    download fcitxDataUrl out "6196053c724125e3ae3d8bd6b2f9172d0c83b65b0d410d3cde63b7a8d6ab87b7"
  "table_txt" ~> do
    let src = "table.tar.gz"
    need [src]
    (Stdout txt) <- cmd "tar" "xfv" src
    produces $ lines txt
  "*.main.dict" %> \out -> do
    let src = takeWhile (/= '.') out
    need ["table_txt"]
    cmd_ "libime_tabledict" src out

--------------------------------------------------------------------------------

download :: String -> FilePath -> String -> Action ()
download baseUrl fileName sha256 = do
  let url = baseUrl <> fileName
  cmd_ "curl" "-LO" url
  if null sha256
    then do
      (Stdout result) <- cmd "sha256sum" fileName
      fail $ "You entered an empty sha256 for " <> fileName <> ". The output of sha256sum is " <> result
    else cmd_ (Stdin $ sha256 <> " " <> fileName) "sha256sum" "--check --status"

--------------------------------------------------------------------------------
