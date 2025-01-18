{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Lua where

import Base
import CMakeBuilder

data Lua = Lua
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult Lua = ()

luaRule :: Rules ()
luaRule = do
  buildLua <-
    useCMake $
      (cmakeBuilder "lua")
        { source = const $ pure "Lua",
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            -- fix build for 32-bit devices
            cmd_ (Cwd src) "git apply ../patches/lua.patch",
          cmakeFlags =
            const
              [ "-DLUA_BUILD_BINARY=OFF",
                "-DLUA_BUILD_COMPILER=OFF",
                "-DLUA_ENABLE_SHARED=OFF",
                "-DLUA_ENABLE_TESTING=OFF",
                "-DLUA_SUPPORT_DL=ON"
              ]
        }
  "lua" ~> buildWithAndroidEnv buildLua Lua
