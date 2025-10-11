{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Image (Image (..)) where

import Build
import Config
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Kernel
import Target
import Utils

data Image = Image

instance Target Image where
  build i = do
    lift $ putStrLn "Building image..."
    imgFile <- artifact i
    kernelFile <- artifact Kernel
    ifM
      (lift $ checkSize kernelFile)
      (dd kernelFile imgFile)
      (throwError "Kernel is too big, aborting compilation")
  deps _ = pure $ Kernel :> NilDeps
  artifact _ = getFromConfig imgFileByConfig

dd :: FilePath -> FilePath -> Build ()
dd kernelFile imgFile = do
  runProcessSilent "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"]
  runProcessSilent "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"]

checkSize :: FilePath -> IO Bool
checkSize = fmap (< kernelSizeKb) . getFileSizeKb
