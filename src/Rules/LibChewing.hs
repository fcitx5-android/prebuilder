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
  let libchewingSrc = "libchewing"
      dictOutputDir = outputDir </> "chewing-dict"
      dictSrcDir = libchewingSrc </> "data"

  buildLibchewing <- addOracle $ \(WithAndroidEnv LibChewing env@AndroidEnv {..}) -> do
    out <- liftIO $ canonicalizePath outputDir
    -- CMakeLists is changed in last build
    cmd_ (Cwd libchewingSrc) Shell "git checkout -- CMakeLists.txt"
    -- skip data and shared lib
    -- merge libuserphrase.a into libchewing.a
    -- remove absolute path by CHEWING_DATADIR macro
    -- remove absolute path by __FILE__ macro
    cmd_ (Cwd libchewingSrc) "git apply ../patches/libchewing.patch"
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

  phony "generateDict" $ do
    cmd_ (Cwd libchewingSrc) "./autogen.sh"
    cmd_ (Cwd libchewingSrc) "./configure --with-sqlite3=no"
    cmd_ (Cwd libchewingSrc) "make"

  libchewingSrc </> "data/*" %> \_ -> need ["generateDict"]

  "chewing-dict" ~> do
    copyFile' (dictSrcDir </> "dictionary.dat") (dictOutputDir </> "dictionary.dat")
    copyFile' (dictSrcDir </> "index_tree.dat") (dictOutputDir </> "index_tree.dat")
    copyFile' (dictSrcDir </> "pinyin.tab") (dictOutputDir </> "pinyin.tab")
    copyFile' (dictSrcDir </> "swkb.dat") (dictOutputDir </> "swkb.dat")
    copyFile' (dictSrcDir </> "symbols.dat") (dictOutputDir </> "symbols.dat")

  "libchewing" ~> do
    need ["chewing-dict"]
    env <- getAndroidEnv
    buildLibchewing $ WithAndroidEnv LibChewing env
