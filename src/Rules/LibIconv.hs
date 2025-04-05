{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibIconv (libiconvRule) where

import Base
import CMakeBuilder

data LibIconv = LibIconv
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibIconv = ()

libiconvRule :: Rules ()
libiconvRule = do
  buildLibIconv <-
    useCMake $
      (cmakeBuilder "libiconv")
        { preBuild = BuildAction $ \_ src -> do
            copyFile' "patches/libiconv/config.h" (src </> "config.h")
            copyFile' "patches/libiconv/CMakeLists.txt" (src </> "CMakeLists.txt")
            cmd_ (Cwd src) "make -f Makefile.devel lib/aliases.h lib/flags.h lib/translit.h"
        }
  "libiconv" ~> buildWithAndroidEnv buildLibIconv LibIconv
