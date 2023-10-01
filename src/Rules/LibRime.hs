{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibRime where

import Base
import Data.List.Extra (intercalate)

data LibRime = LibRime
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibRime = ()

librimeRule :: Rules ()
librimeRule = do
  buildLibrime <- addOracle $ \(WithAndroidEnv LibRime env@AndroidEnv {..}) -> do
    let librimeSrc = "librime"
    -- canocialize for symlink
    librimeLuaSrc <- liftIO $ canonicalizePath "librime-lua"
    librimeOctagramSrc <- liftIO $ canonicalizePath "librime-octagram"
    out <- liftIO $ canonicalizePath outputDir
    liftIO $ do
      removePathForcibly (librimeSrc </> "plugins" </> "lua")
      createDirectoryLink librimeLuaSrc (librimeSrc </> "plugins" </> "lua")
      removePathForcibly (librimeSrc </> "plugins" </> "octagram")
      createDirectoryLink librimeOctagramSrc (librimeSrc </> "plugins" </> "octagram")
    cmd_ (Cwd (librimeSrc </> "plugins" </> "lua")) Shell "sed -i '11s|^\\s*if(LUA_FOUND)|set(LUA_FOUND 1)\\nset(LUA_INCLUDE_DIRS \"${CMAKE_CURRENT_SOURCE_DIR}/../build/lua/${ANDROID_ABI}/include\")\\n\\0|' CMakeLists.txt"
    cmd_ (Cwd (librimeSrc </> "plugins" </> "octagram")) Shell "sed -i '18s|^add_subdirectory.*||' CMakeLists.txt"
    -- remove absolute path by __FILE__ macro
    cmd_ (Cwd librimeSrc) Shell "sed -i '143s|\\(^.*target_link_libraries.*\\)|target_compile_options\\(rime-static PRIVATE \"-ffile-prefix-map=${CMAKE_CURRENT_SOURCE_DIR}=.\"\\)\\n\\1|' src/CMakeLists.txt"
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "librime" </> a
        let buildDir = out </> "librime-build-" <> a
        cmd_
          (Cwd librimeSrc)
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
            "-DCMAKE_FIND_ROOT_PATH="
              <> intercalate
                ";"
                ( map
                    (\x -> out </> x </> a)
                    [ "boost",
                      "glog",
                      "yaml-cpp",
                      "leveldb",
                      "marisa",
                      "opencc"
                    ]
                    <> ["."]
                ),
            "-DBUILD_SHARED_LIBS=OFF",
            "-DBUILD_STATIC=ON",
            "-DBUILD_TEST=OFF",
            "-DCMAKE_CXX_FLAGS=-DBOOST_DISABLE_CURRENT_LOCATION"
          ]
        cmd_ (Cwd librimeSrc) cmake "--build" buildDir
        cmd_ (Cwd librimeSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/librime.a"
        removeFilesAfter outPrefix ["lib/pkgconfig"]
  "librime" ~> do
    need
      [ "opencc",
        "boost",
        "glog",
        "yaml-cpp",
        "leveldb",
        "marisa"
      ]
    env <- getAndroidEnv
    buildLibrime $ WithAndroidEnv LibRime env
