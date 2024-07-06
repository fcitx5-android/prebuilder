{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibThai (libthaiRule) where

import Base
import CMakeBuilder

data LibThai = LibThai
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibThai = ()

libthaiRule :: Rules ()
libthaiRule = do
  buildLibThai <-
    useCMake $
      (cmakeBuilder "libthai")
        { preBuild = BuildAction $ \_ src -> do
            copyFile' "patches/libthai/CMakeLists.txt" (src </> "CMakeLists.txt")
        }
  "libthai" ~> buildWithAndroidEnv buildLibThai LibThai
