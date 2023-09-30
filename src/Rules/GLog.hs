{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.GLog (glogRule) where

import Base

data GLog = GLog
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GLog = ()

glogRule :: Rules ()
glogRule = do
  buildGlog <- addOracle $ \(WithAndroidEnv GLog env@AndroidEnv {..}) -> do
    let glogSrc = "glog"
    -- remove absolute path by __FILE__ macro
    cmd_ (Cwd glogSrc) Shell "sed -i '618s|\\(^add_library (glog.*\\)|target_compile_options\\(glogbase PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\1|' CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = outputDir </> "glog" </> a
        let buildDir = outputDir </> "glog-build-" <> a
        cmd_
          (Cwd glogSrc)
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
            "-DWITH_GFLAGS=OFF",
            "-DWITH_UNWIND=OFF",
            "-DBUILD_TESTING=OFF"
          ]
        cmd_ (Cwd glogSrc) cmake "--build" buildDir
        cmd_ (Cwd glogSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libglog.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "glog" ~> do
    env <- getAndroidEnv
    buildGlog $ WithAndroidEnv GLog env
