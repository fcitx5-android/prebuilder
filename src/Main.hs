{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Base
import Data.Aeson qualified as A
import Data.Aeson.Encode.Pretty qualified as A
import Data.List.Extra (dropWhileEnd, find, isPrefixOf, split)
import Data.String (fromString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Development.Shake.Config
import Rules.AnthyData
import Rules.Boost
import Rules.Fcitx5Data
import Rules.Fmt
import Rules.GLog
import Rules.LevelDB
import Rules.LibEvent
import Rules.LibIntlLite
import Rules.LibRime
import Rules.Lua
import Rules.Marisa
import Rules.OpenCC
import Rules.YAMLCpp

--------------------------------------------------------------------------------
main :: IO ()
main = do
  shakeArgs
    shakeOptions
      { shakeReport = ["report.html"],
        shakeVersion = prebuilderVersion,
        shakeFiles = outputDir
      }
    $ do
      usingConfigFile "build.cfg"
      downloadFileRule
      spellDictRule
      libimeRule
      chineseAddonsRule
      fmtRule
      libeventRule
      libintlLiteRule
      luaRule
      openccRule
      boostRule
      glogRule
      yamlCppRule
      leveldbRule
      marisaRule
      librimeRule
      anthyDictRule
      "everything" ~> do
        let artifacts =
              [ "spell-dict",
                "libime",
                "fmt",
                "chinese-addons-data",
                "libevent",
                "libintl-lite",
                "lua",
                "opencc",
                "boost",
                "glog",
                "yaml-cpp",
                "leveldb",
                "marisa",
                "librime",
                "anthy-dict"
              ]
        need artifacts
        writeFileLines (outputDir </> "artifacts.txt") ("toolchain-versions.json" : artifacts)
        getToolchainVersions >>= writeFile' (outputDir </> "toolchain-versions.json") . TL.unpack . TLB.toLazyText . A.encodePrettyToTextBuilder
      "clean" ~> do
        removeFilesAfter outputDir ["//*"]
        cmd_ "git" "submodule" "foreach" "--recursive" "git" "reset" "--hard"
        cmd_ "git" "submodule" "foreach" "--recursive" "git" "clean" "-x" "-f" "-d"

--------------------------------------------------------------------------------

data ToolchainVersions = ToolchainVersions
  { prebuilderRev :: String,
    ndkVersion :: String,
    platformVersion :: Int,
    cmakeVersion :: String
  }

instance A.ToJSON ToolchainVersions where
  toJSON ToolchainVersions {..} =
    A.object
      [ fromString "prebuilder" A..= prebuilderRev,
        fromString "ndk" A..= ndkVersion,
        fromString "platform" A..= platformVersion,
        fromString "cmake" A..= cmakeVersion
      ]

getToolchainVersions :: Action ToolchainVersions
getToolchainVersions = do
  StdoutTrim prebuilderRev <- cmd "git" "rev-parse" "HEAD"
  AndroidEnv {sdkCmakeVersion = cmakeVersion, platform = platformVersion, ..} <- getAndroidEnv
  properties <- readFileLines $ ndkRoot </> "source.properties"
  ndkVersion <- case find ("Pkg.Revision" `isPrefixOf`) properties of
    Just line
      | [_, ndkVersion] <- split (== '=') line ->
          pure $ dropWhileEnd (== ' ') ndkVersion
      | otherwise -> fail "Failed to parse Pkg.Revision"
    Nothing -> fail "Pkg.Revision not found in source.properties"
  pure ToolchainVersions {..}
