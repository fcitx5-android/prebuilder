{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.GLog (glogRule) where

import Base
import CMakeBuilder
import Control.Arrow ((>>>))

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
          -- remove absolute path by __FILE__ macro
          preBuild = \_ src -> cmd_ (Cwd src) Shell "sed -i '618s|\\(^add_library (glog.*\\)|target_compile_options\\(glogbase PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\1|' CMakeLists.txt",
          postBuildEachABI = stripLib "lib/libglog.a" >>> removePkgConfig
        }
  "glog" ~> buildWithAndroidEnv buildGlog GLog
