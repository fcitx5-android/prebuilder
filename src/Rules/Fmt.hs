{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Fmt (fmtRule) where

import Base
import CMakeBuilder

data Fmt = Fmt
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Fmt = ()

fmtRule :: Rules ()
fmtRule = do
  buildFmt <-
    useCMake
      (cmakeBuilder "fmt")
        { cmakeFlags = const ["-DFMT_TEST=OFF", "-DFMT_DOC=OFF"]
        }
  "fmt" ~> buildWithAndroidEnv buildFmt Fmt
