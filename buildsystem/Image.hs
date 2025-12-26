{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Image (Image (..)) where

import Build
import Config
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Foldable
import KernelElf
import System.Process (proc)
import Target
import UserspaceProgram
import Utils

data Image = Image

instance Target Image where
  build i = do
    imgFile <- artifact i
    kernelFile <- artifact KernelElf
    userspaceBins <- traverseDeps artifact userspace
    ifM
      (lift $ checkSize kernelFile)
      (dd kernelFile imgFile userspaceBins)
      (throwError "Kernel is too big, aborting compilation")
  deps _ = pure (KernelElf :> userspace)
  artifact _ = getFromConfig imgFileByConfig
  name _ = Just "image"

userspace :: Deps
userspace = depsFromList UserspaceProgram userspacePrograms

dd :: FilePath -> FilePath -> [FilePath] -> Build ()
dd kernelFile imgFile userspaceBins = do
  runProcessSilent (proc "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"])
  runProcessSilent (proc "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"])
  traverse_ (uncurry concatProgram) (userspaceBins `zip` offsets)
  where
    offsets = [firstProgramOffset + n * programSizeBlocks | n <- [0 ..]]
    firstProgramOffset = 194
    programSizeBlocks = 128
    concatProgram :: FilePath -> Int -> Build ()
    concatProgram bin offset =
      runProcessSilent $
        proc "dd" ["if=" ++ bin, "of=" ++ imgFile, "seek=" ++ show offset, "count=" ++ show programSizeBlocks]

checkSize :: FilePath -> IO Bool
checkSize = fmap (< kernelSizeKb) . getFileSizeKb
