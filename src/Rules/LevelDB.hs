{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LevelDB (leveldbRule) where

import Base
import CMakeBuilder

data LevelDB = LevelDB
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LevelDB = ()

leveldbRule :: Rules ()
leveldbRule = do
  buildLevelDB <-
    useCMake $
      (cmakeBuilder "leveldb")
        { cmakeFlags =
            const
              [ "-DBUILD_SHARED_LIBS=OFF",
                "-DLEVELDB_BUILD_BENCHMARKS=OFF",
                "-DLEVELDB_BUILD_TESTS=OFF"
              ]
        }
  "leveldb" ~> buildWithAndroidEnv buildLevelDB LevelDB
