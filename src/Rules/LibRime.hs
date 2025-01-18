{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibRime where

import Base
import CMakeBuilder
import Data.List.Extra (intercalate)

data LibRime = LibRime
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibRime = ()

librimeRule :: Rules ()
librimeRule = do
  buildLibrime <-
    useCMake $
      (cmakeBuilder "librime")
        { preBuild = BuildAction $ \_ src -> do
            -- canocialize for symlink
            librimeLuaSrc <- liftIO $ canonicalizePath "librime-lua"
            librimeOctagramSrc <- liftIO $ canonicalizePath "librime-octagram"
            librimePredictSrc <- liftIO $ canonicalizePath "librime-predict"
            liftIO $ do
              removePathForcibly (src </> "plugins" </> "lua")
              createDirectoryLink librimeLuaSrc (src </> "plugins" </> "lua")
              removePathForcibly (src </> "plugins" </> "octagram")
              createDirectoryLink librimeOctagramSrc (src </> "plugins" </> "octagram")
              removePathForcibly (src </> "plugins" </> "predict")
              createDirectoryLink librimePredictSrc (src </> "plugins" </> "predict")
            -- find lua with find_package; remove absolute path by __FILE__ macro
            cmd_ (Cwd librimeLuaSrc) "git checkout ."
            cmd_ (Cwd librimeLuaSrc) "git apply ../patches/librime-lua.patch"
            -- disable tools; remove absolute path by __FILE__ macro
            cmd_ (Cwd librimeOctagramSrc) "git checkout ."
            cmd_ (Cwd librimeOctagramSrc) "git apply ../patches/librime-octagram.patch"
            -- disable tools; remove absolute path by __FILE__ macro
            cmd_ (Cwd librimePredictSrc) "git checkout ."
            cmd_ (Cwd librimePredictSrc) "git apply ../patches/librime-predict.patch"
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/librime.patch",
          cmakeFlags = \BuildEnv {..} ->
            [ "-DBUILD_SHARED_LIBS=OFF",
              "-DBUILD_STATIC=ON",
              "-DBUILD_TEST=OFF",
              "-DBUILD_TOOLS=OFF",
              "-DALSO_LOG_TO_STDERR=ON",
              "-DCMAKE_FIND_ROOT_PATH="
                <> intercalate
                  ";"
                  ( map
                      (\x -> buildEnvOut </> x </> buildEnvABI)
                      [ "boost",
                        "glog",
                        "yaml-cpp",
                        "leveldb",
                        "marisa",
                        "opencc",
                        "lua"
                      ]
                      <> ["."]
                  )
            ]
        }
  "librime" ~> do
    need
      [ "lua",
        "opencc",
        "boost",
        "glog",
        "yaml-cpp",
        "leveldb",
        "marisa"
      ]
    buildWithAndroidEnv buildLibrime LibRime
