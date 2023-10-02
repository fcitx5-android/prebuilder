{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibIntlLite (libintlLiteRule) where

import Base

data LibIntlLite = LibIntlLite
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibIntlLite = ()

libintlLiteRule :: Rules ()
libintlLiteRule = do
  buildLibintlLite <- addOracle $ \(WithAndroidEnv LibIntlLite env@AndroidEnv {..}) -> do
    let libintlSrc = "libintl-lite"
    out <- liftIO $ canonicalizePath outputDir
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "libintl-lite" </> a
        let buildDir = out </> "libintl-lite-build-" <> a
        cmd_
          (Cwd libintlSrc)
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
            "-DCMAKE_BUILD_TYPE=Release"
          ]
        cmd_ (Cwd libintlSrc) cmake "--build" buildDir
        cmd_ (Cwd libintlSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libintl.a"
  "libintl-lite" ~> do
    env <- getAndroidEnv
    buildLibintlLite $ WithAndroidEnv LibIntlLite env
