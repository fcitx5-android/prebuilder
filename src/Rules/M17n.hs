{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.M17n (m17nRule) where

import Base
import CMakeBuilder

data M17n = M17n
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult M17n = ()

m17nRule :: Rules ()
m17nRule = do
  buildM17n <-
    useCMake $
      (cmakeBuilder "m17n-lib")
        { source = const $ pure "m17n-cmake",
          preBuild = BuildAction $ \_ src -> do
            let m17nLibSrc = src </> "m17n-lib"
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/m17n/m17n.patch"
            cmd_ (Cwd m17nLibSrc) "git checkout ."
            cmd_ (Cwd m17nLibSrc) "git apply ../patches/m17n-lib.patch",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF"]
        }

  "m17n-db" ~> do
    let m17nDbSrc = "m17n-cmake/m17n-db"
    let m17nDbDst = outputDir </> "m17n-db"
    cmd_ (Cwd m17nDbSrc) Shell "./bootstrap.sh"
    cmd_ (Cwd m17nDbSrc) "./configure --with-charmaps=../../glibc/localedata/charmaps --prefix=/usr"
    cmd_ (Cwd m17nDbSrc) "make"
    cmd_ (Cwd m17nDbSrc) "make install DESTDIR=/tmp"
    cmd_ ("rm -rf " <> m17nDbDst)
    cmd_ ("cp -r /tmp/usr/share/m17n " <> m17nDbDst)

  "m17n-lib" ~> do
    buildWithAndroidEnv buildM17n M17n
