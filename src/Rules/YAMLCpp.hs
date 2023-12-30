{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.YAMLCpp (yamlCppRule) where

import Base
import CMakeBuilder

data YamlCpp = YamlCpp
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult YamlCpp = ()

yamlCppRule :: Rules ()
yamlCppRule = do
  buildYamlCpp <-
    useCMake $
      (cmakeBuilder "yaml-cpp")
        { cmakeFlags =
            const
              [ "-DBUILD_SHARED_LIBS=OFF",
                "-DYAML_CPP_BUILD_CONTRIB=OFF",
                "-DYAML_CPP_BUILD_TESTS=OFF",
                "-DYAML_CPP_BUILD_TOOLS=OFF"
              ]
        }
  "yaml-cpp" ~> buildWithAndroidEnv buildYamlCpp YamlCpp
