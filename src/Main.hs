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
import Rules.LibChewing
import Rules.LibUV
import Rules.LibIconv
import Rules.LibHangul
import Rules.LibIME
import Rules.LibIMEJyutping
import Rules.LibIntlLite
import Rules.LibMozc
import Rules.LibRime
import Rules.LibThai
import Rules.Lua
import Rules.Marisa
import Rules.OpenCC
import Rules.YAMLCpp
import Rules.ZSTD

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- don't use shakeArgs since it applies withoutActions on our rule
  shakeArgsWith
    shakeOptions
      { shakeReport = ["report.html"],
        shakeVersion = prebuilderVersion,
        shakeFiles = outputDir
      }
    []
    $ \_ files -> pure . Just $ do
      want files
      usingConfigFile "build.cfg"
      downloadFileRule
      spellDictRule
      libIMERule
      chineseAddonsRule
      libIMEJyutpingRule
      fmtRule
      libuvRule
      libintlLiteRule
      luaRule
      openccRule
      boostRule
      zstdRule
      hostLibzstdRule
      glogRule
      yamlCppRule
      leveldbRule
      marisaRule
      librimeRule
      libhangulRule
      libchewingRule
      libthaiRule
      libiconvRule
      anthyDictRule
      libmozcRule
      isInGitHubActionRule
      getOutputDirRule
      "everything" ~> do
        let artifacts =
              [ "spell-dict",
                "libime",
                "fmt",
                "chinese-addons-data",
                "libime-jyutping",
                "libuv",
                "libintl-lite",
                "lua",
                "opencc",
                "boost",
                "zstd",
                "glog",
                "yaml-cpp",
                "leveldb",
                "marisa",
                "libmozc",
                "librime",
                "libhangul",
                "chewing-dict",
                "libchewing",
                "libthai",
                "libiconv",
                "anthy-dict"
              ]
        need artifacts
        writeFileLines (outputDir </> "artifacts.txt") ("toolchain-versions.json" : artifacts)
        getToolchainVersions >>= writeFile' (outputDir </> "toolchain-versions.json") . TL.unpack . TLB.toLazyText . A.encodePrettyToTextBuilder
      "clean" ~> do
        removeFilesAfter outputDir ["//*"]
        cmd_ "git" "submodule" "foreach" "--recursive" "git" "reset" "--hard"
        cmd_ "git" "submodule" "foreach" "--recursive" "git" "clean" "-x" "-f" "-d"

      action $
        whenM isInGitHubAction $
          writeGitHubBuildSummary ["### Build Summary :rocket:"]

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
  AndroidEnv {sdkCMakeVersion = cmakeVersion, platform = platformVersion, ..} <- getAndroidEnv
  properties <- readFileLines $ ndkRoot </> "source.properties"
  ndkVersion <- case find ("Pkg.Revision" `isPrefixOf`) properties of
    Just line
      | [_, ndkVersion] <- split (== '=') line ->
          pure $ dropWhileEnd (== ' ') ndkVersion
      | otherwise -> fail "Failed to parse Pkg.Revision"
    Nothing -> fail "Pkg.Revision not found in source.properties"
  pure ToolchainVersions {..}
