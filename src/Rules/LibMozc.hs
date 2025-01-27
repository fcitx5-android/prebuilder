{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibMozc where

import Base
import CMakeBuilder

data LibMozc = LibMozc
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibMozc = ()

libmozcRule :: Rules ()
libmozcRule = do
  buildLibmozc <-
    useCMake $
      (cmakeBuilder "libmozc")
        { preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "cmake -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DBUILD_MOZC_ADDON=OFF"
            cmd_ (Cwd src) "cmake --build build --target protoc",
          cmakeFlags =
            const
              [ "-DPROTOC_EXECUTABLE=../../build/protoc",
                "-DCMAKE_CXX_FLAGS=-I../../libmozc/mozc/src/bazel-bin",
                "-DBUILD_MOZC_ADDON=OFF"
              ],
          postBuildEachABI = BuildActionABI $ \_ BuildEnv {..} -> do
            cmd_ (Cwd buildEnvBuildDir) Shell "ar rc" (buildEnvOutPrefix </> "lib" </> "libabsl.a") "$(find mozc/src/third_party/abseil-cpp -name '*.o')"
        }
  "libmozc" ~> buildWithAndroidEnv buildLibmozc LibMozc
