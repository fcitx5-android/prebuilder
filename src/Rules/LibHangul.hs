{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Rules.LibHangul where

import Base
import CMakeBuilder

data LibHangul = LibHangul
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LibHangul = ()

libhangulRule :: Rules ()
libhangulRule = do
  buildLibhangul <-
    useCMake $
      (cmakeBuilder "libhangul")
        { preBuild = \_ src -> do
            -- disable executable
            cmd_ (Cwd src) Shell "sed -i '53s|add_subdirectory(tools)||' CMakeLists.txt"
            cmd_ (Cwd src) Shell "sed -i '48s|FULL_LOCALEDIR|LOCALEDIR|' hangul/CMakeLists.txt"
            cmd_ (Cwd src) Shell "sed -i '49,50s|FULL_DATADIR|DATADIR|' hangul/CMakeLists.txt",
          cmakeFlags = const ["-DBUILD_SHARED_LIBS=OFF", "-DENABLE_EXTERNAL_KEYBOARDS=OFF"],
          postBuildEachABI = stripLib "lib/libhangul.a"
        }
  "libhangul" ~> do
    env <- getAndroidEnv
    let abiList = getABIList env
        firstAbi = head abiList
    -- delete old data and symlinks
    liftIO $ do
      removePathForcibly $ outputDir </> "libhangul" </> "data"
      forM_ abiList $ \a -> do
        removePathForcibly $ outputDir </> "libhangul" </> a </> "share"
    buildLibhangul $ WithAndroidEnv LibHangul env
    liftIO $ do
      getDirectoryFilesIO
        (outputDir </> "libhangul" </> firstAbi </> "share")
        ["//*"]
        >>= mapM_
          ( \x ->
              copyFileAndCreateDir (outputDir </> "libhangul" </> firstAbi </> "share" </> x) $
                outputDir </> "libhangul" </> "data" </> x
          )
      forM_ abiList $ \a -> do
        -- symlink dictionaries for each abi to reduce size
        let dataPath = outputDir </> "libhangul" </> a </> "share"
        whenM (doesPathExist dataPath) $ removePathForcibly dataPath
        createDirectoryLink (".." </> "data") dataPath
