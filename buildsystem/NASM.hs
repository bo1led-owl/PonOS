module NASM (NASM (..)) where

import Config
import File
import System.FilePath
import System.Process (proc)
import Target
import Utils

data NASM = NASM Mod FilePath

instance Target NASM where
  build n@(NASM m f) = do
    output <- artifact n
    runProcess $ proc "nasm" (args output)
    where
      args output = (if m == Kernel then constants else []) ++ ["-felf32", f, "-o", output]
  deps (NASM _ f) = pure (File f :> NilDeps)
  artifact (NASM m f) = pure (((`replaceDirectory` outdirMod m) . (<.> "o")) f)
  name (NASM _ f) = Just f
