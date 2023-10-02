{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.ZSTD (zstdRule) where

import Base

data ZSTD = ZSTD
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult ZSTD = ()

zstdRule :: Rules ()
zstdRule = do
  buildZSTD <- addOracle $ \(WithAndroidEnv ZSTD env@AndroidEnv {..}) -> do
    let zstdSrc = "zstd"
    out <- liftIO $ canonicalizePath outputDir
    cmd_ (Cwd zstdSrc) Shell "sed -i '137s|set_target_properties(.*|target_compile_options(libzstd_static PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\")\\n\\0|' build/cmake/lib/CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "zstd" </> a
        let buildDir = out </> "zstd-build-" <> a
        cmd_
          (Cwd zstdSrc)
          cmake
          "-B"
          buildDir
          "-GNinja"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DCMAKE_MAKE_PROGRAM=" <> ninja,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DZSTD_LEGACY_SUPPORT=OFF",
            "-DZSTD_BUILD_PROGRAMS=OFF",
            "-DZSTD_BUILD_TESTS=OFF",
            "-DZSTD_BUILD_SHARED=OFF"
          ]
          ("build" </> "cmake")
        cmd_ (Cwd zstdSrc) cmake "--build" buildDir
        cmd_ (Cwd zstdSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libzstd.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "zstd" ~> do
    env <- getAndroidEnv
    buildZSTD $ WithAndroidEnv ZSTD env
