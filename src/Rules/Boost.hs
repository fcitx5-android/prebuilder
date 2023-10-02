{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Boost (boostRule) where

import Base
import Data.List.Extra (replace)

newtype Boost = Boost {boostLib :: String}
  deriving stock (Show, Typeable, Eq, Generic)
  deriving newtype (Hashable, Binary, NFData)

type instance RuleResult Boost = ()

boostRule :: Rules ()
boostRule = do
  buildBoost <- addOracle $ \(WithAndroidEnv Boost {..} AndroidEnv {..}) -> do
    let boostAndroidSrc = "Boost-for-Android"
    boostVersion <- getConfig' "boost_version"
    sha256 <- getConfig' "boost_sha256"
    let boostTar = "boost_" <> replace "." "_" boostVersion <.> "tar" <.> "bz2"
        boostUrl = "https://boostorg.jfrog.io/artifactory/main/release/" <> boostVersion <> "/source/"
    _ <- download boostUrl boostTar sha256
    -- this script always clean the build directory before building
    cmd_
      (Cwd outputDir)
      (".." </> boostAndroidSrc </> "build-android.sh")
      [ "--boost=" <> boostVersion,
        "--with-libraries=" <> boostLib,
        "--arch=" <> abi,
        "--target-version=" <> show platform,
        "--layout="
      ]
      ndkRoot
  "boost" ~> do
    env <- getAndroidEnv
    -- since header files are the same regardless of abi
    -- we take a random one
    let abiList = getABIList env
        firstAbi = head abiList
    buildBoost $ WithAndroidEnv (Boost "filesystem,iostreams,regex,system") env
    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "build" </> "out" </> firstAbi </> "include" </> "boost")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "build" </> "out" </> firstAbi </> "include" </> "boost" </> x) $
                outputDir </> "boost" </> "include" </> "boost" </> x
          )
      forM_ abiList $ \a -> do
        getDirectoryFilesIO
          (outputDir </> "build" </> "out" </> a </> "lib")
          ["*.a", "//*.cmake"]
          >>= mapM_ (\x -> copyFileAndCreateDir (outputDir </> "build" </> "out" </> a </> "lib" </> x) $ outputDir </> "boost" </> a </> "lib" </> x)
        -- symlink headers for each abi to reduce size
        let path = outputDir </> "boost" </> a </> "include"
        whenM (doesPathExist path) $ removePathForcibly path
        createDirectoryLink (".." </> "include") path
