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
    forM_ (getRustTargetTriples androidEnv) $ \target ->
      cmd_ "rustup" "target" "add" target
