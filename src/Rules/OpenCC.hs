{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.OpenCC (openccRule) where

import Base

data OpenCC = OpenCC
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpenCC = ()

openccRule :: Rules ()
openccRule = do
  buildOpenCC <- addOracle $ \(WithAndroidEnv OpenCC env@AndroidEnv {..}) -> do
    let openccSrc = "OpenCC"
    out <- liftIO $ canonicalizePath outputDir
    -- use prebuilt marisa
    cmd_ (Cwd openccSrc) Shell "sed -i '213s|find_library(LIBMARISA NAMES marisa)|find_package(marisa)\\nset(LIBMARISA marisa)|' CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "opencc" </> a
        let buildDir = out </> "opencc-build-" <> a
        cmd_
          (Cwd openccSrc)
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
            "-DSHARE_INSTALL_PREFIX=share",
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
            "-Dmarisa_DIR=" <> (out </> "marisa" </> a </> "lib" </> "cmake" </> "marisa"),
            "-DUSE_SYSTEM_PYBIND11=OFF",
            "-DUSE_SYSTEM_RAPIDJSON=OFF",
            "-DUSE_SYSTEM_TCLAP=OFF"
          ]
        cmd_ (Cwd openccSrc) cmake "--build" buildDir
        cmd_ (Cwd openccSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libopencc.a"
        removeFilesAfter outPrefix ["bin", "lib/pkgconfig"]
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
