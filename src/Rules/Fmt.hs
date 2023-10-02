{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Fmt (fmtRule) where

import Base

data Fmt = Fmt
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Fmt = ()

fmtRule :: Rules ()
fmtRule = do
  buildFmt <- addOracle $ \(WithAndroidEnv Fmt env@AndroidEnv {..}) -> do
    let fmtSrc = "fmt"
    out <- liftIO $ canonicalizePath outputDir
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "fmt" </> a
        let buildDir = out </> "fmt-build-" <> a
        cmd_
          (Cwd fmtSrc)
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
            "-DFMT_TEST=OFF",
            "-DFMT_DOC=OFF"
          ]
        cmd_ (Cwd fmtSrc) cmake "--build" buildDir
        cmd_ (Cwd fmtSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libfmt.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "fmt" ~> do
    env <- getAndroidEnv
    buildFmt $ WithAndroidEnv Fmt env
