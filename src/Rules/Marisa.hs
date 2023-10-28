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
          preBuild = \_ src -> cmd_ (Cwd src) Shell "sed -i '52s|install(TARGETS marisa.*|target_compile_options\\(marisa PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\0|' CMakeLists.txt",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF"]
        }
  "marisa" ~> buildWithAndroidEnv buildMarisa MarisaTrie
