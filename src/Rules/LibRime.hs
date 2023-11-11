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
            liftIO $ do
              removePathForcibly (src </> "plugins" </> "lua")
              createDirectoryLink librimeLuaSrc (src </> "plugins" </> "lua")
              removePathForcibly (src </> "plugins" </> "octagram")
              createDirectoryLink librimeOctagramSrc (src </> "plugins" </> "octagram")
            -- use prebuilt lua
            cmd_ (Cwd (src </> "plugins" </> "lua")) Shell "sed -i '11s|^\\s*if(LUA_FOUND)|set(LUA_FOUND 1)\\nset(LUA_INCLUDE_DIRS \"${CMAKE_CURRENT_SOURCE_DIR}/../build/lua/${ANDROID_ABI}/include\")\\n\\0|' CMakeLists.txt"
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd (src </> "plugins" </> "lua")) Shell "sed -i '47s|^set(plugin_name.*|target_compile_options(rime-lua-objs PRIVATE \"-ffile-prefix-map=${CMAKE_SOURCE_DIR}=.\")\\n\\0|' CMakeLists.txt"
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd (src </> "plugins" </> "octagram")) Shell "sed -i '13s|^set(plugin_name.*|target_compile_options(rime-octagram-objs PRIVATE \"-ffile-prefix-map=${CMAKE_SOURCE_DIR}=.\")\\n\\0|' CMakeLists.txt"
            -- remove command line tools
            cmd_ (Cwd (src </> "plugins" </> "octagram")) Shell "sed -i 19{/add_subdirectory\\(tools\\)/d} CMakeLists.txt"
            -- remove absolute path by __FILE__ macro
            cmd_ (Cwd src) Shell "sed -i '143s|target_link_libraries(rime-static.*|target_compile_options(rime-static PRIVATE \"-ffile-prefix-map=${CMAKE_SOURCE_DIR}=.\")\\n\\0|' src/CMakeLists.txt",
          cmakeFlags = \BuildEnv {..} ->
            [ "-DBUILD_SHARED_LIBS=OFF",
              "-DBUILD_STATIC=ON",
              "-DBUILD_TEST=OFF",
              "-DCMAKE_CXX_FLAGS=-DBOOST_DISABLE_CURRENT_LOCATION",
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
                        "opencc"
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
