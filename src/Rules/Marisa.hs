{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.Marisa
  ( marisaRule,
    hostMarisaRule,
  )
where

import Base
import CMakeBuilder

data MarisaTrie = MarisaTrie
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MarisaTrie = ()

hostMarisaRule :: Rules ()
hostMarisaRule = do
  "host-marisa" ~> do
    marisaSrc <- liftIO $ canonicalizePath "marisa-trie"
    let buildDir = outputDir </> "marisa-build-host"
    let hostPrefix = outputDir </> "host"
    cmd_ (Cwd marisaSrc) "git checkout ."
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
          preBuild = BuildAction $ \_ src -> do
            cmd_ (Cwd src) "git checkout ."
            cmd_ (Cwd src) "git apply ../patches/marisa-tire.patch",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF"]
        }
  "marisa" ~> buildWithAndroidEnv buildMarisa MarisaTrie
