module Rules.RustToolchain
  ( hostRustToolchainRule,
    androidRustTargetsRule,
  )
where

import Base

hostRustToolchainRule :: Rules ()
hostRustToolchainRule = do
  "host-rust-toolchain" ~> do
    androidEnv <- getAndroidEnv
    cmd_ "rustup" "toolchain" "install" (rustVersion androidEnv)

androidRustTargetsRule :: Rules ()
androidRustTargetsRule = do
  "android-rust-targets" ~> do
    need ["host-rust-toolchain"]
    androidEnv <- getAndroidEnv
    forM_ (getABIList androidEnv) $ \a -> do
      let target = case a of
            "armeabi-v7a" -> "armv7-linux-androideabi"
            "arm64-v8a" -> "aarch64-linux-android"
            "x86_64" -> "x86_64-linux-android"
            "x86" -> "i686-linux-android"
            _ -> error $ "Unknown Android ABI: " <> a
      cmd_ "rustup" "target" "add" target
