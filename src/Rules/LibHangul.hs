{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibHangul where

import Base

data LibHangul = LibHangul
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibHangul = ()

libhangulRule :: Rules ()
libhangulRule = do
  buildLibhangul <- addOracle $ \(WithAndroidEnv LibHangul env@AndroidEnv {..}) -> do
    let libhangulSrc = "libhangul"
    out <- liftIO $ canonicalizePath outputDir
    -- disable executable
    cmd_ (Cwd libhangulSrc) Shell "sed -i '53s|add_subdirectory(tools)||' CMakeLists.txt"
    cmd_ (Cwd libhangulSrc) Shell "sed -i '48s|FULL_LOCALEDIR|LOCALEDIR|' hangul/CMakeLists.txt"
    cmd_ (Cwd libhangulSrc) Shell "sed -i '49,50s|FULL_DATADIR|DATADIR|' hangul/CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libhangul" </> a
        let buildDir = out </> "libhangul-build-" <> a
        cmd_
          (Cwd libhangulSrc)
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
            "-DBUILD_SHARED_LIBS=OFF",
            "-DENABLE_EXTERNAL_KEYBOARDS=OFF"
          ]
        cmd_ (Cwd libhangulSrc) cmake "--build" buildDir
        cmd_ (Cwd libhangulSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libhangul.a"
  "libhangul" ~> do
    env <- getAndroidEnv
    let abiList = getABIList env
        firstAbi = head abiList
    -- delete old data and symlinks
    liftIO $ do
      removePathForcibly $ outputDir </> "libhangul" </> "data"
      forM_ abiList $ \a -> do
        removePathForcibly $ outputDir </> "libhangul" </> a </> "share"
    buildLibhangul $ WithAndroidEnv LibHangul env
    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "libhangul" </> firstAbi </> "share")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "libhangul" </> firstAbi </> "share" </> x) $
                outputDir </> "libhangul" </> "data" </> x
          )
      forM_ abiList $ \a -> do
        -- symlink dictionaries for each abi to reduce size
        let dataPath = outputDir </> "libhangul" </> a </> "share"
        whenM (doesPathExist dataPath) $ removePathForcibly dataPath
        createDirectoryLink (".." </> "data" ) dataPath
