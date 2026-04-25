{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.OpenCC
  ( hostOpenCCRule,
    openccRule,
  )
where

import Base
import CMakeBuilder

data OpenCC = OpenCC
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult OpenCC = ()

hostOpenCCRule :: Rules ()
hostOpenCCRule = do
  "opencc-dict" ~> do
    need [ "host-marisa" ]
    openccSrc <- liftIO $ canonicalizePath "OpenCC"
    let buildDir = outputDir </> "opencc-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_ Shell ("echo " <> openccSrc)
    cmd_ (Cwd openccSrc) "git checkout ."
    cmd_ (Cwd openccSrc) "git apply ../patches/opencc-host.patch"
    cmd_
      "cmake"
      "-B" buildDir
      "-G" "Ninja"
      [ "-DCMAKE_INSTALL_PREFIX=" <> (outputDir </> "opencc"),
        "-DCMAKE_FIND_ROOT_PATH=" <> hostPrefix,
        "-DCMAKE_PREFIX_PATH=" <> hostPrefix
      ]
      openccSrc
    cmd_ "cmake" "--build" buildDir "--target" [ "opencc_dict", "Dictionaries" ]
    cmd_ "cmake" "--install" buildDir "--component" "data"

openccRule :: Rules ()
openccRule = do
  buildOpenCC <-
    useCMake $
      (cmakeBuilder "opencc")
        { source = const $ pure "OpenCC",
          -- use prebuilt marisa
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/opencc.patch",
          cmakeFlags = \BuildEnv {..} ->
            [ "-DSHARE_INSTALL_PREFIX=share",
              "-DINCLUDE_INSTALL_DIR=include",
              "-DSYSCONF_INSTALL_DIR=etc",
              "-DLIB_INSTALL_DIR=lib",
              "-DBUILD_SHARED_LIBS=OFF",
              "-DBUILD_DOCUMENTATION=OFF",
              "-DBUILD_PYTHON=OFF",
              "-DENABLE_GTEST=OFF",
              "-DENABLE_BENCHMARK=OFF",
              "-DENABLE_DARTS=OFF",
              "-DUSE_SYSTEM_MARISA=ON",
              "-DMarisa_DIR=" <> (buildEnvOut </> "marisa" </> buildEnvABI </> "lib" </> "cmake" </> "Marisa"),
              "-DUSE_SYSTEM_PYBIND11=OFF",
              "-DUSE_SYSTEM_RAPIDJSON=OFF",
              "-DUSE_SYSTEM_TCLAP=OFF"
            ]
        }
  "opencc" ~> do
    need [ "marisa", "opencc-dict" ]
    buildWithAndroidEnv buildOpenCC OpenCC
