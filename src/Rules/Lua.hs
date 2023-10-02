{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Lua where

import Base

data Lua = Lua
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Lua = ()

luaRule :: Rules ()
luaRule = do
  buildLua <- addOracle $ \(WithAndroidEnv Lua env@AndroidEnv {..}) -> do
    let luaSrc = "Lua"
    out <- liftIO $ canonicalizePath outputDir
    withAndroidEnv env $ \cmake toolchain ninja strip abiList ->
      forM_ abiList $ \a -> do
        let outPrefix = out </> "lua" </> a
        let buildDir = out </> "lua-build-" <> a
        cmd_
          (Cwd luaSrc)
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
            "-DLUA_BUILD_BINARY=OFF",
            "-DLUA_BUILD_COMPILER=OFF",
            "-DLUA_ENABLE_SHARED=OFF",
            "-DLUA_ENABLE_TESTING=OFF",
            "-DLUA_SUPPORT_DL=ON"
          ]
        cmd_ (Cwd luaSrc) cmake "--build" buildDir
        cmd_ (Cwd luaSrc) cmake "--install" buildDir
        cmd_ (Cwd outPrefix) strip "--strip-unneeded" "lib/liblua_static.a"
  "lua" ~> do
    env <- getAndroidEnv
    buildLua $ WithAndroidEnv Lua env
