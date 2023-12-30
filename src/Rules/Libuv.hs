{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Libuv (libuvRule) where

import Base
import CMakeBuilder

data Libuv = Libuv
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Libuv = ()

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
  "libuv" ~> buildWithAndroidEnv buildlibuv Libuv
