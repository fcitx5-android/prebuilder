# prebuilder

Build static libraries for [fcitx5-android](https://github.com/fcitx5-android/fcitx5-android).

## Build environment

Cabal is required to build this project.

### Android related

* Android Platform (API Level): `23`
* Android NDK: `28.0.13004108`
* CMake Version: `3.31.6`

both can be installed with `sdkmanager` command line tool.

### System dependencies

* `tar curl git`
* `ghc haskell-shake haskell-aeson-pretty cabal-install`: haskell stuff
* `cmake extra-cmake-modules ninja`: cmake stuff
* `python`: build dep of `opencc`
* `opencc`: for `opencc_dict`
* `gperf`: build dep of `libiconv`

## Libraries

* boost ([cmake build](https://github.com/boostorg/cmake)): [boostorg/boost](https://github.com/boostorg/boost)
* libuv: [libuv/libuv](https://github.com/libuv/libuv)
* libintl-lite: [j-jorge/libintl-lite](https://github.com/j-jorge/libintl-lite)
* fmt: [fmtlib/fmt](https://github.com/fmtlib/fmt)
* fcitx5: [fcitx/fcitx5](https://github.com/fcitx/fcitx5)
* zstd: [facebook/zstd](https://github.com/facebook/zstd)
* libime data: [fcitx/libime](https://github.com/fcitx/libime)
* libime-jyutping data: [fcitx/libime-jyutping](https://github.com/fcitx/libime-jyutping)
* lua (cmake build): [walterschell/Lua](https://github.com/walterschell/Lua)
* opencc: [BYVoid/OpenCC](https://github.com/BYVoid/OpenCC)
* anthy-unicode: [fujiwarat/anthy-unicode](https://github.com/fujiwarat/anthy-unicode)
* glog: [google/glog](https://github.com/google/glog)
* yaml-cpp: [jbeder/yaml-cpp](https://github.com/jbeder/yaml-cpp)
* leveldb: [google/leveldb](https://github.com/google/leveldb)
* marisa-trie: [s-yata/marisa-trie](https://github.com/s-yata/marisa-trie)
* librime: [rime/librime](https://github.com/rime/librime)
* librime-lua: [rime/librime-lua](https://github.com/hchunhui/librime-lua)
* librime-octagram: [rime/librime-octagram](https://github.com/lotem/librime-octagram)
* libhangul: [libhangul/libhangul](https://github.com/libhangul/libhangul)
* libchewing: [chewing/libchewing](https://github.com/chewing/libchewing)
* libthai: [tlwg/libthai](https://github.com/tlwg/libthai)
* libiconv: [GNU/libiconv](https://savannah.gnu.org/projects/libiconv)
