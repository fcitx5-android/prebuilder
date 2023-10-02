{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.YAMLCpp (yamlCppRule) where

import Base

data YamlCpp = YamlCpp
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult YamlCpp = ()

yamlCppRule :: Rules ()
yamlCppRule = do
  buildYamlCpp <- addOracle $ \(WithAndroidEnv YamlCpp env@AndroidEnv {..}) -> do
    let yamlCppSrc = "yaml-cpp"
    out <- liftIO $ canonicalizePath outputDir
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "yaml-cpp" </> a
        let buildDir = out </> "yaml-cpp-build-" <> a
        cmd_
          (Cwd yamlCppSrc)
          cmake
          "-B"
          buildDir
          "-GNinja"
          [ "-DCMAKE_TOOLCHAIN_FILE=" <> toolchain,
            "-DCMAKE_MAKE_PROGRAM=" <> ninja,
            "-DANDROID_ABI=" <> a,
            "-DANDROID_PLATFORM=" <> show platform,
            "-DANDROID_STL=c++_shared",
            "-DCMAKE_INSTALL_PREFIX=" <> outPrefix,
            "-DCMAKE_BUILD_TYPE=Release",
            "-DBUILD_SHARED_LIBS=OFF",
            "-DYAML_CPP_BUILD_CONTRIB=OFF",
            "-DYAML_CPP_BUILD_TESTS=OFF",
            "-DYAML_CPP_BUILD_TOOLS=OFF"
          ]
        cmd_ (Cwd yamlCppSrc) cmake "--build" buildDir
        cmd_ (Cwd yamlCppSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/libyaml-cpp.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "yaml-cpp" ~> do
    env <- getAndroidEnv
    buildYamlCpp $ WithAndroidEnv YamlCpp env
