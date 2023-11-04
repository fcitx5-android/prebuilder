{
  description = "Dev shell flake for fcitx5-android prebuilder";

  inputs.fcitx5-android.url = "github:fcitx5-android/fcitx5-android";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, fcitx5-android, ... }:
    let
      nixpkgs = fcitx5-android.inputs.nixpkgs;
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.android_sdk.accept_license = true;
        config.allowUnfree = true;
        overlays = [ fcitx5-android.overlays.default ];
      };
    in with pkgs;
    let
      sdk = pkgs.fcitx5-android.sdk;
      prebuilder = haskellPackages.callPackage ./nix { };
      prebuilderShell = (haskell.lib.addBuildTools prebuilder [
        haskell-language-server
        cabal2nix
        cabal-install
      ]).env;
    in {
      devShells.x86_64-linux.default = (sdk.shell.override {
        androidStudio = null;
        generateLocalProperties = false;
      }).overrideAttrs (old: {
        ABI = "armeabi-v7a,arm64-v8a,x86,x86_64";
        ANDROID_PLATFORM = sdk.platformVersion;
        CMAKE_VERSION = sdk.cmakeVersion;
        shellHook = ''
          ${old.shellHook}
          export ANDROID_NDK_ROOT="$ANDROID_SDK_ROOT/ndk/${sdk.ndkVersion}"
          export COMP_SPELL_DICT="${fcitx5}/lib/fcitx5/libexec/comp-spell-dict"
        '';

        buildInputs = old.buildInputs ++ [
          fcitx5
          opencc
          autoconf
          automake
          pkg-config
          libtool
          check
          zstd
          boost
        ] ++ prebuilderShell.buildInputs;

        nativeBuildInputs = old.nativeBuildInputs
          ++ prebuilderShell.nativeBuildInputs;

      });
      packages.x86_64-linux.default =
        haskell.lib.justStaticExecutables prebuilder;
    };
}
