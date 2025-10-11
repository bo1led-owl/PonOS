module Loader (Loader (..)) where

import Config
import Control.Monad.Reader
import File
import System.FilePath
import Target
import Utils

data Loader = Loader

instance Target Loader where
  build Loader = do
    lift $ putStrLn "Building loader..."
    output <- artifact Loader
    runProcess "nasm" (args output)
    where
      args output = ["-felf32", "-dKERNEL_SIZE_KB=" ++ show kernelSizeKb, loaderSource, "-o", output]
  deps Loader = pure $ File loaderSource :> NilDeps
  artifact Loader = pure (outdir </> "loader.o")

loaderSource :: FilePath
loaderSource = srcdir </> "loader.nasm"