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
          preBuild = BuildAction $ \_ src -> cmd_ (Cwd src) Shell "sed -i '137s|set_target_properties(.*|target_compile_options(libzstd_static PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\")\\n\\0|' build/cmake/lib/CMakeLists.txt",
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
    cmd_
      "cmake"
      "-B"
      (zstdSrc </> "build-host")
      "-G"
      "Ninja"
      [ "-DCMAKE_BUILD_TYPE=Release",
        "-DCMAKE_INSTALL_PREFIX=" <> outputDir,
        "-DZSTD_LEGACY_SUPPORT=OFF",
        "-DZSTD_BUILD_PROGRAMS=OFF",
        "-DZSTD_BUILD_TESTS=OFF"
      ]
      (zstdSrc </> "build" </> "cmake")
    cmd_
      "cmake"
      "--build"
      (zstdSrc </> "build-host")
    cmd_
      "cmake"
      "--install"
      (zstdSrc </> "build-host")
