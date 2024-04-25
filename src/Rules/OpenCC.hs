{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.OpenCC (openccRule) where

import Base
import CMakeBuilder

data OpenCC = OpenCC
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpenCC = ()

openccRule :: Rules ()
openccRule = do
  buildOpenCC <-
    useCMake $
      (cmakeBuilder "opencc")
        { source = const $ pure "OpenCC",
          -- use prebuilt marisa
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/opencc.patch",
          cmakeFlags = \BuildEnv {..} ->
            [ "-DSHARE_INSTALL_PREFIX=share",
              "-DINCLUDE_INSTALL_DIR=include",
              "-DSYSCONF_INSTALL_DIR=etc",
              "-DLIB_INSTALL_DIR=lib",
              "-DBUILD_SHARED_LIBS=OFF",
              "-DBUILD_DOCUMENTATION=OFF",
              "-DBUILD_PYTHON=OFF",
              "-DENABLE_GTEST=OFF",
              "-DENABLE_BENCHMARK=OFF",
              "-DENABLE_DARTS=OFF",
              "-DUSE_SYSTEM_MARISA=ON",
              "-Dmarisa_DIR=" <> (buildEnvOut </> "marisa" </> buildEnvABI </> "lib" </> "cmake" </> "marisa"),
              "-DUSE_SYSTEM_PYBIND11=OFF",
              "-DUSE_SYSTEM_RAPIDJSON=OFF",
              "-DUSE_SYSTEM_TCLAP=OFF"
            ]
        }
  "opencc" ~> do
    need ["marisa"]
    env <- getAndroidEnv
    -- since dictionary files are the same regardless of abi
    -- we take a random one
    let abiList = getABIList env
        firstAbi = head abiList
    -- delete old data and symlinks
    liftIO $ do
      removePathForcibly $ outputDir </> "opencc" </> "data"
      forM_ abiList $ \a -> removePathForcibly $ outputDir </> "opencc" </> a </> "share" </> "opencc"
    buildOpenCC $ WithAndroidEnv OpenCC env
    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "opencc" </> firstAbi </> "share" </> "opencc")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "opencc" </> firstAbi </> "share" </> "opencc" </> x) $
                outputDir </> "opencc" </> "data" </> x
          )
      forM_ abiList $ \a -> do
        -- symlink dictionaries for each abi to reduce size
        let dataPath = outputDir </> "opencc" </> a </> "share" </> "opencc"
        whenM (doesPathExist dataPath) $ removePathForcibly dataPath
        createDirectoryLink (".." </> ".." </> "data") dataPath
