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
          -- remove absolute path by __FILE__ macro
          preBuild = BuildAction $ \_ src -> cmd_ (Cwd src) Shell "sed -i '405s|\\(^add_library (glog.*\\)|target_compile_options\\(glog_internal PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\1|' CMakeLists.txt"
        }
  "glog" ~> buildWithAndroidEnv buildGlog GLog
