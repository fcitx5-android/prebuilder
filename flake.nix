{
  description = "Dev shell flake for fcitx5-android prebuilder";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, nixpkgs, ... }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.android_sdk.accept_license = true;
        config.allowUnfree = true;
        overlays = [ self.overlays.default ];
      };
    in with pkgs;
    with fcitx5-android-sdk;
    {
      devShells.x86_64-linux.default = mkShell rec {
        buildInputs = [
          androidComposition.androidsdk
          extra-cmake-modules
          gettext
          python39
          icu
          libime
          opencc
          haskell-language-server
          (haskellPackages.ghcWithPackages
            (pkgs: with pkgs; [ shake aeson-pretty ]))
          autoconf
          automake
          pkg-config
          libtool
        ];
        ANDROID_SDK_ROOT =
          "${androidComposition.androidsdk}/libexec/android-sdk";
        ANDROID_NDK_ROOT = "${ANDROID_SDK_ROOT}/ndk/${ndkVersion}";
        CMAKE_VERSION = cmakeVersion;
        ABI = lib.head abiVersions;
        ANDROID_PLATFORM = platformVersion;
        JAVA_HOME = "${jdk11}";
        shellHook = ''
          export PATH="$ANDROID_SDK_ROOT/cmake/${cmakeVersion}/bin:$PATH"
        '';
      };
    } // {
      overlays.default = final: prev: {
        fcitx5-android-sdk = rec {
          cmakeVersion = "3.22.1";
          buildToolsVersion = "33.0.2";
          platformToolsVersion = "33.0.3";
          platformVersion = "33";
          ndkVersion = "25.2.9519653";
          abiVersions = [ "arm64-v8a" ];
          androidComposition = prev.androidenv.composeAndroidPackages {
            inherit platformToolsVersion ndkVersion;
            buildToolsVersions = [ buildToolsVersion ];
            platformVersions = [ platformVersion ];
            inherit abiVersions;
            cmakeVersions = [ cmakeVersion ];
            includeNDK = true;
            includeEmulator = false;
          };
        };
      };
    };
}
