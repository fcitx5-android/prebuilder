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
            copyFile' "patches/libiconv/config.h" (src </> "include/config.h")
            copyFile' "patches/libiconv/CMakeLists.txt" (src </> "CMakeLists.txt")
            copyFile' (src </> "libcharset/include/localcharset.h.in") (src </> "libcharset/include/localcharset.h")
            cmd_ (Cwd src) "make -f Makefile.devel lib/aliases.h lib/flags.h lib/translit.h"
            copyFile' (src </> "include/iconv.h.in") (src </> "include/iconv.h")
            cmd_ (Cwd src) "git apply ../patches/libiconv/libiconv.patch"
        }
  "libiconv" ~> buildWithAndroidEnv buildLibIconv LibIconv
