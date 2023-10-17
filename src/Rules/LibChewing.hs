{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibChewing where

import Base

data LibChewing = LibChewing
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibChewing = ()

libchewingRule :: Rules ()
libchewingRule = do
  buildLibchewing <- addOracle $ \(WithAndroidEnv LibChewing env@AndroidEnv {..}) -> do
    let libchewingSrc = "libchewing"
    out <- liftIO $ canonicalizePath outputDir
    cmd_ (Cwd libchewingSrc) Shell "git checkout -- CMakeLists.txt"
    -- merge libuserphrase.a into libchewing.a
    cmd_ (Cwd libchewingSrc) Shell "sed -i '400s|STATIC|OBJECT|' CMakeLists.txt"
    -- skip data and shared lib
    cmd_ (Cwd libchewingSrc) Shell "sed -i '171,189d;232,241d;328d;412,417d;455,456d' CMakeLists.txt"
    -- remove absolute path by __FILE__ macro
    cmd_ (Cwd libchewingSrc) Shell "sed -i '334s|set_target_properties(chewing.*|target_compile_options(chewing PRIVATE \"-ffile-prefix-map=${CMAKE_SOURCE_DIR}=.\")\\n\\0|' CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libchewing" </> a
        let buildDir = out </> "libchewing-build-" <> a
        cmd_
          (Cwd libchewingSrc)
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
            "-DWITH_SQLITE3=OFF"
          ]
        cmd_ (Cwd libchewingSrc) cmake "--build" buildDir
        cmd_ (Cwd libchewingSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libchewing.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "libchewing" ~> do
    need ["chewing-dict"]
    env <- getAndroidEnv
    buildLibchewing $ WithAndroidEnv LibChewing env
