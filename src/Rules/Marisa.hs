{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Marisa
  ( marisaRule,
    hostMarisaRule,
    marisaSourceRule,
  )
where

import Base
import CMakeBuilder

data MarisaTrie = MarisaTrie
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MarisaTrie = ()

marisaSourceRule :: Rules ()
marisaSourceRule = do
  "marisa-source" ~> do
    src <- liftIO $ canonicalizePath "marisa-trie"
    cmd_ (Cwd src) "git checkout ."
    cmd_ (Cwd src) "git apply ../patches/marisa-tire.patch"

hostMarisaRule :: Rules ()
hostMarisaRule = do
  "host-marisa" ~> do
    need ["marisa-source"]
    marisaSrc <- liftIO $ canonicalizePath "marisa-trie"
    let buildDir = outputDir </> "marisa-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_
      "cmake"
      "-B" buildDir
      "-G" "Ninja"
      [ "-DCMAKE_INSTALL_PREFIX=" <> hostPrefix
      ]
      marisaSrc
    cmd_ "cmake" "--build" buildDir
    cmd_ "cmake" "--install" buildDir

marisaRule :: Rules ()
marisaRule = do
  buildMarisa <-
    useCMake $
      (cmakeBuilder "marisa")
        { source = const $ pure "marisa-trie",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF"]
        }
  "marisa" ~> do
    need ["marisa-source"]
    buildWithAndroidEnv buildMarisa MarisaTrie
