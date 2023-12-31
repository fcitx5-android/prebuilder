{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibUV (libuvRule) where

import Base
import CMakeBuilder

data LibUV = LibUV
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibUV = ()

libuvRule :: Rules ()
libuvRule = do
  buildlibuv <-
    useCMake
      (cmakeBuilder "libuv")
        { cmakeFlags =
            const
              [ "-DLIBUV_BUILD_SHARED=OFF",
                "-DLIBUV_BUILD_TESTS=OFF",
                "-DLIBUV_BUILD_BENCH=OFF"
              ]
        }
  "libuv" ~> buildWithAndroidEnv buildlibuv LibUV
