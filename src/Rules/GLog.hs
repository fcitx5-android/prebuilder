{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.GLog (glogRule) where

import Base
import CMakeBuilder

data GLog = GLog
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GLog = ()

glogRule :: Rules ()
glogRule = do
  buildGlog <-
    useCMake $
      (cmakeBuilder "glog")
        { cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF", "-DWITH_GFLAGS=OFF", "-DWITH_UNWIND=OFF", "-DBUILD_TESTING=OFF"],
          -- disable logging to cwd; remove absolute path by __FILE__ macro
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/glog.patch"
        }
  "glog" ~> buildWithAndroidEnv buildGlog GLog
