{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Marisa (marisaRule) where

import Base

data MarisaTrie = MarisaTrie
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MarisaTrie = ()

marisaRule :: Rules ()
marisaRule = do
  buildMarisa <- addOracle $ \(WithAndroidEnv MarisaTrie env@AndroidEnv {..}) -> do
    let marisaSrc = "marisa-trie"
    out <- liftIO $ canonicalizePath outputDir
    cmd_ (Cwd marisaSrc) Shell "sed -i '42s|\\(^install.*\\)|target_compile_options\\(marisa PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\1|' CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "marisa" </> a
        let buildDir = out </> "marisa-build-" <> a
        cmd_
          (Cwd marisaSrc)
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
            "-DBUILD_SHARED_LIBS=OFF"
          ]
        cmd_ (Cwd marisaSrc) cmake "--build" buildDir
        cmd_ (Cwd marisaSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libmarisa.a"
  "marisa" ~> do
    env <- getAndroidEnv
    buildMarisa $ WithAndroidEnv MarisaTrie env
