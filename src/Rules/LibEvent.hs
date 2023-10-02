{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibEvent (libeventRule) where

import Base

data LibEvent = LibEvent
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibEvent = ()

libeventRule :: Rules ()
libeventRule = do
  buildLibevent <- addOracle $ \(WithAndroidEnv LibEvent env@AndroidEnv {..}) -> do
    let libeventSrc = "libevent"
    out <- liftIO $ canonicalizePath outputDir
    -- make cmake generate relative _IMPORT_PREFIX
    cmd_ (Cwd libeventSrc) "sed -i 1456s|${CMAKE_INSTALL_PREFIX}/|| CMakeLists.txt"
    cmd_ (Cwd libeventSrc) "sed -i 1475{\\|\"${PROJECT_SOURCE_DIR}/include\"|d} CMakeLists.txt"
    cmd_ (Cwd libeventSrc) "sed -i 1475s|${PROJECT_BINARY_DIR}/|| CMakeLists.txt"
    -- fix LibeventConfig.cmake find_{path,library} calls in ndk toolchain
    cmd_ (Cwd libeventSrc) Shell "sed -i '120s|NO_DEFAULT_PATH)|NO_DEFAULT_PATH NO_CMAKE_FIND_ROOT_PATH)|' cmake/LibeventConfig.cmake.in"
    cmd_ (Cwd libeventSrc) Shell "sed -i '134s|NO_DEFAULT_PATH)|NO_DEFAULT_PATH NO_CMAKE_FIND_ROOT_PATH)|' cmake/LibeventConfig.cmake.in"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libevent" </> a
        let buildDir = out </> "libevent-build-" <> a
        cmd_
          (Cwd libeventSrc)
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
            "-DEVENT__LIBRARY_TYPE=STATIC",
            "-DEVENT__DISABLE_DEBUG_MODE=ON",
            "-DEVENT__DISABLE_THREAD_SUPPORT=ON",
            "-DEVENT__DISABLE_OPENSSL=ON",
            "-DEVENT__DISABLE_BENCHMARK=ON",
            "-DEVENT__DISABLE_TESTS=ON",
            "-DEVENT__DISABLE_REGRESS=ON",
            "-DEVENT__DISABLE_SAMPLES=ON"
          ]
        cmd_ (Cwd libeventSrc) cmake "--build" buildDir
        -- avoid void installing pkgconf files and python scripts
        cmd_ (Cwd libeventSrc) cmake "--install" buildDir "--component" "lib"
        cmd_ (Cwd libeventSrc) cmake "--install" buildDir "--component" "dev"
        cmd_ (Cwd outPrefix) Shell strip "--strip-unneeded" "lib/libevent*.a"
  "libevent" ~> do
    env <- getAndroidEnv
    buildLibevent $ WithAndroidEnv LibEvent env
