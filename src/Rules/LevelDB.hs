{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LevelDB (leveldbRule) where

import Base

data LevelDB = LevelDB
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LevelDB = ()

leveldbRule :: Rules ()
leveldbRule = do
  buildLevelDB <- addOracle $ \(WithAndroidEnv LevelDB env@AndroidEnv {..}) -> do
    let leveldbSrc = "leveldb"
    out <- liftIO $ canonicalizePath outputDir
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "leveldb" </> a
        let buildDir = out </> "leveldb-build-" <> a
        cmd_
          (Cwd leveldbSrc)
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
            "-DLEVELDB_BUILD_BENCHMARKS=OFF",
            "-DLEVELDB_BUILD_TESTS=OFF"
          ]
        cmd_ (Cwd leveldbSrc) cmake "--build" buildDir
        cmd_ (Cwd leveldbSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libleveldb.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "leveldb" ~> do
    env <- getAndroidEnv
    buildLevelDB $ WithAndroidEnv LevelDB env
