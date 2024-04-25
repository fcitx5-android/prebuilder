{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Marisa (marisaRule) where

import Base
import CMakeBuilder

data MarisaTrie = MarisaTrie
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MarisaTrie = ()

marisaRule :: Rules ()
marisaRule = do
  buildMarisa <-
    useCMake $
      (cmakeBuilder "marisa")
        { source = const $ pure "marisa-trie",
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/marisa-tire.patch",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF"]
        }
  "marisa" ~> buildWithAndroidEnv buildMarisa MarisaTrie
