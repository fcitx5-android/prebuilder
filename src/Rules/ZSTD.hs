{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.ZSTD (zstdRule) where

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
              ],
          postBuildEachABI = stripLib "lib/libzstd.a" <> removePkgConfig
        }
  "zstd" ~> buildWithAndroidEnv buildZSTD ZSTD
