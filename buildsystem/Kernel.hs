{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Kernel (Kernel (..)) where

import Build
import Clang
import Config
import Control.Monad.Reader
import Loader
import System.Directory
import System.FilePath
import System.Process (proc)
import Target
import Utils

data Kernel = Kernel

instance Target Kernel where
  build Kernel = do
    config <- getConfig
    lift $ createDirectoryIfMissing True (outdirByConfig config)
    loaderO <- artifact Loader
    cObjects <- lift cFiles >>= traverse artifact
    let objs = loaderO : cObjects
    linkKernel config objs
  deps Kernel = do
    c <- depsFromList id <$> lift cFiles
    pure (Loader :> c)
  artifact Kernel = getFromConfig kernelBin
  name Kernel = Just "kernel"

cFiles :: IO [Clang]
cFiles = map Clang <$> getFilesWithExtensions srcdir [".c"]

linkKernel :: Config -> [FilePath] -> Build ()
linkKernel config objs = do
  ld kernelElf objs
  runProcess $ proc "objcopy" ["-I", "elf32-i386", "-O", "binary", kernelElf, kernelBin config]
  where
    kernelElf = kernelElfByConfig config

ld :: FilePath -> [FilePath] -> Build ()
ld output = runProcess . proc "ld.lld" . (["-e", "kernelEntry", "-T", "link.ld", "-o", output] ++)

kernelBin :: Config -> FilePath
kernelBin config = outdirByConfig config </> "kernel.bin"
