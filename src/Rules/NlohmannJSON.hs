{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.NlohmannJSON (nlohmannJSONRule) where

import Base
import CMakeBuilder

data NlohmannJSON = NlohmannJSON
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult NlohmannJSON = ()

nlohmannJSONRule :: Rules ()
nlohmannJSONRule = do
  buildNlohmannJSON <-
    useCMake
      (cmakeBuilder "nlohmann_json")
        { cmakeFlags =
            const
              [ "-DBUILD_TESTING=OFF",
                "-DJSON_BuildTests=OFF",
                -- install boost headers to parent directory, symlink it afterwards
                "-DCMAKE_INSTALL_INCLUDEDIR=" <> "../include"
              ],
          -- symlink headers for each abi to reduce size
          postBuildEachABI = BuildActionABI $ \_ env ->
            liftIO $ do
              let includePath = buildEnvOutPrefix env </> "include"
              whenM (doesPathExist includePath) $ removePathForcibly includePath
              createDirectoryLink (".." </> "include") includePath
        }
  "nlohmann_json" ~> buildWithAndroidEnv buildNlohmannJSON NlohmannJSON
