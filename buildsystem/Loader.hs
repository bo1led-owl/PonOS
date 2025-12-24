module Loader (Loader (..)) where

import Config
import Control.Monad.Trans
import File
import System.FilePath
import System.Process (proc)
import Target
import Utils

data Loader = Loader

instance Target Loader where
  build Loader = do
    output <- artifact Loader
    runProcess $ proc "nasm" (args output)
    where
      args output = constants ++ ["-felf32", loaderSource, "-o", output]
  deps Loader = (File loaderSource :>) <$> lift (depsFromList id <$> nasmFiles)
  artifact Loader = pure (outdir </> "loader.o")
  name Loader = Just "loader"

nasmFiles :: IO [File]
nasmFiles = map File <$> getFilesWithExtensions srcdir [".nasm"]

loaderSource :: FilePath
loaderSource = srcdir </> "loader.nasm"