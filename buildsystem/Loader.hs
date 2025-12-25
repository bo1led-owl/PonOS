module Loader (Loader (..)) where

import Config
import Control.Monad.Trans
import File
import NASM
import System.FilePath
import Target
import Utils

data Loader = Loader

instance Target Loader where
  build Loader = build loader
  deps Loader = ((<>) . depsFromList id <$> lift nasmFiles) <*> deps loader
  artifact Loader = artifact loader

loader :: NASM
loader = NASM Kernel (kernelSrcDir </> "loader.nasm")

nasmFiles :: IO [File]
nasmFiles = map File <$> getFilesWithExtensions kernelSrcDir [".nasm"]
