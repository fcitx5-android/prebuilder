{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibEvent (libeventRule) where

import Base
import CMakeBuilder

data LibEvent = LibEvent
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibEvent = ()

libeventRule :: Rules ()
libeventRule = do
  buildLibevent <-
    useCMake $
      (cmakeBuilder "libevent")
        { preBuild = BuildAction $ \_ src -> do
            -- make cmake generate relative _IMPORT_PREFIX
            cmd_ (Cwd src) "sed -i 1456s|${CMAKE_INSTALL_PREFIX}/|| CMakeLists.txt"
            cmd_ (Cwd src) "sed -i 1475{\\|\"${PROJECT_SOURCE_DIR}/include\"|d} CMakeLists.txt"
            cmd_ (Cwd src) "sed -i 1475s|${PROJECT_BINARY_DIR}/|| CMakeLists.txt"
            -- fix LibeventConfig.cmake find_{path,library} calls in ndk toolchain
            cmd_ (Cwd src) Shell "sed -i '120s|NO_DEFAULT_PATH)|NO_DEFAULT_PATH NO_CMAKE_FIND_ROOT_PATH)|' cmake/LibeventConfig.cmake.in"
            cmd_ (Cwd src) Shell "sed -i '134s|NO_DEFAULT_PATH)|NO_DEFAULT_PATH NO_CMAKE_FIND_ROOT_PATH)|' cmake/LibeventConfig.cmake.in",
          cmakeFlags =
            const
              [ "-DEVENT__LIBRARY_TYPE=STATIC",
                "-DEVENT__DISABLE_DEBUG_MODE=ON",
                "-DEVENT__DISABLE_THREAD_SUPPORT=ON",
                "-DEVENT__DISABLE_OPENSSL=ON",
                "-DEVENT__DISABLE_BENCHMARK=ON",
                "-DEVENT__DISABLE_TESTS=ON",
                "-DEVENT__DISABLE_REGRESS=ON",
                "-DEVENT__DISABLE_SAMPLES=ON"
              ],
          -- we install manually in post build below
          doInstall = False,
          postBuildEachABI =
            mconcat
              [ BuildActionABI $ \_ BuildEnv {..} ->
                  do
                    let cmake = getSdkCMake buildEnvAndroid
                    -- avoid void installing pkgconf files and python scripts
                    cmd_ (Cwd buildEnvSrc) cmake "--install" buildEnvBuildDir "--component" "lib"
                    cmd_ (Cwd buildEnvSrc) cmake "--install" buildEnvBuildDir "--component" "dev",
                stripLib "lib/libevent*.a"
              ]
        }
  "libevent" ~> buildWithAndroidEnv buildLibevent LibEvent
