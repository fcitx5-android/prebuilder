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
                "-DBOOST_EXCLUDE_LIBRARIES="
                  <> intercalate
                    ";"
                    [ "atomic",
                      "charconv",
                      "chrono",
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
              ]
        }
  "boost" ~> do
    env <- getAndroidEnv
    -- since header files are the same regardless of abi
    -- we take a random one
    let abiList = getABIList env
        firstAbi = head abiList
    _ <- buildBoost $ WithAndroidEnv Boost env
    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "boost" </> firstAbi </> "include" </> "boost")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "boost" </> firstAbi </> "include" </> "boost" </> x) $
                outputDir </> "boost" </> "include" </> "boost" </> x
          )
      forM_ abiList $ \a -> do
        -- symlink headers for each abi to reduce size
        let path = outputDir </> "boost" </> a </> "include"
        whenM (doesPathExist path) $ removePathForcibly path
        createDirectoryLink (".." </> "include") path
