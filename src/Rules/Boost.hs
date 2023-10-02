{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Boost (boostRule) where

import Base
import Data.List.Extra (intercalate)

data Boost = Boost
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <- addOracle $ \(WithAndroidEnv Boost env@AndroidEnv {..}) -> do
    boostVersion <- getConfig' "boost_version"
    sha256 <- getConfig' "boost_sha256"
    out <- liftIO $ canonicalizePath outputDir
    let boostTag = "boost-" <> boostVersion
        boostTar = boostTag <.> "tar" <.> "xz"
        boostUrl = "https://github.com/boostorg/boost/releases/download/" </> boostTag </> boostTar 
    _ <- download boostUrl boostTar sha256
    cmd_
      (Cwd outputDir)
      "tar" "xf" boostTar
    let boostSrc = out </> boostTag
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "boost" </> a
        let buildDir = out </> "boost-build-" <> a
        cmd_
          (Cwd boostSrc)
          cmake
          "-B"
          buildDir
          "-GNinja"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DCMAKE_MAKE_PROGRAM=" <> ninja,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_BUILD_TYPE=Release",
            "-DBOOST_EXCLUDE_LIBRARIES="
              <> intercalate
                ";"
                [ "chrono",
                  "context",
                  "contract",
                  "coroutine",
                  "date_time",
                  "exception",
                  "fiber",
                  "graph",
                  "json",
                  "locale",
                  "log",
                  "math",
                  "nowide",
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
            "-DBOOST_INSTALL_LAYOUT=system"
          ]
        cmd_ (Cwd boostSrc) cmake "--build" buildDir
        cmd_ (Cwd boostSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) Shell strip "--strip-unneeded" "lib/libboost_*.a"
  "boost" ~> do
    env <- getAndroidEnv
    -- since header files are the same regardless of abi
    -- we take a random one
    let abiList = getABIList env
        firstAbi = head abiList
    buildBoost $ WithAndroidEnv Boost env
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
