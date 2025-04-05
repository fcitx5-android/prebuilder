{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.ZSTD
  ( zstdRule,
    hostLibzstdRule,
  )
where

import Base
import CMakeBuilder

data ZSTD = ZSTD
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult ZSTD = ()

zstdRule :: Rules ()
zstdRule = do
  buildZSTD <-
    useCMake $
      (cmakeBuilder "zstd")
        { cmakeFile = Just $ "build" </> "cmake",
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/zstd.patch",
          cmakeFlags =
            const
              [ "-DZSTD_LEGACY_SUPPORT=OFF",
                "-DZSTD_BUILD_PROGRAMS=OFF",
                "-DZSTD_BUILD_TESTS=OFF",
                "-DZSTD_BUILD_SHARED=OFF"
              ]
        }
  "zstd" ~> buildWithAndroidEnv buildZSTD ZSTD

hostLibzstdRule :: Rules ()
hostLibzstdRule = do
  "host-libzstd" ~> do
    let zstdSrc = "zstd"
    let buildDir = outputDir </> "zstd-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_
      "cmake"
      "-B"
      buildDir
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix,
        "-DZSTD_LEGACY_SUPPORT=OFF",
        "-DZSTD_BUILD_PROGRAMS=OFF",
        "-DZSTD_BUILD_TESTS=OFF"
      ]
      (zstdSrc </> "build" </> "cmake")
    cmd_
      "cmake"
      "--build"
      buildDir
    cmd_
      "cmake"
      "--install"
      buildDir
