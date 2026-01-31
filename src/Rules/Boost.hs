{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Boost (boostRule) where

import Base
import CMakeBuilder
import Data.List.Extra (intercalate)

data Boost = Boost
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <-
    useCMake $
      (cmakeBuilder "boost")
        { source = \out -> do
            boostVersion <- getConfig' "boost_version"
            sha256 <- getConfig' "boost_sha256"
            let boostTag = "boost-" <> boostVersion
                boostTar = boostTag <> "-cmake" <.> "tar" <.> "xz"
                boostUrl = "https://github.com/boostorg/boost/releases/download" </> boostTag <> "/"
            _ <- download boostUrl boostTar sha256
            cmd_ (Cwd out) "tar" "xf" boostTar
            pure $ out </> boostTag,
          cmakeFlags =
            const
              [ "-DCMAKE_INSTALL_MESSAGE=NEVER",
                -- install boost headers to parent directory, symlink it afterwards
                "-DCMAKE_INSTALL_INCLUDEDIR=" <> "../include",
                "-DBOOST_EXCLUDE_LIBRARIES="
                  <> intercalate
                    ";"
                    [ "atomic",
                      "asio",
                      "charconv",
                      "chrono",
                      "cobalt",
                      "context",
                      "contract",
                      "coroutine",
                      "date_time",
                      "fiber",
                      "filesystem",
                      "graph",
                      "json",
                      "locale",
                      "log",
                      "math",
                      "nowide",
                      "process",
                      "program_options",
                      "serialization",
                      "stacktrace",
                      "test",
                      "thread",
                      "timer",
                      "type_erasure",
                      "url",
                      "wave",
                      "wserialization"
                    ],
                "-DBOOST_IOSTREAMS_ENABLE_BZIP2=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_ZLIB=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_LZMA=OFF",
                "-DBOOST_IOSTREAMS_ENABLE_ZSTD=OFF",
                "-DBOOST_INSTALL_LAYOUT=system"
              ],
          -- symlink headers for each abi to reduce size
          postBuildEachABI = BuildActionABI $ \_ env ->
            liftIO $ do
              let includePath = buildEnvOutPrefix env </> "include"
              whenM (doesPathExist includePath) $ removePathForcibly includePath
              createDirectoryLink (".." </> "include") includePath
        }
  "boost" ~> do
    buildWithAndroidEnv buildBoost Boost
