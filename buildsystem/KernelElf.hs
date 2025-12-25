{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module KernelElf (KernelElf (..)) where

import Build
import Clang
import Config
import Control.Monad.Reader
import Loader
import System.FilePath
import System.Process (proc)
import Target
import Utils

data KernelElf = KernelElf

instance Target KernelElf where
  build KernelElf = do
    config <- getConfig
    loaderO <- artifact Loader
    cObjects <- lift cFiles >>= traverse artifact
    let objs = loaderO : cObjects
    linkKernel config objs
  deps KernelElf = do
    c <- depsFromList id <$> lift cFiles
    pure (Loader :> c)
  artifact KernelElf = getFromConfig kernelBin
  name KernelElf = Just "kernel"

cFiles :: IO [Clang]
cFiles = map (Clang Kernel) <$> getFilesWithExtensions kernelSrcDir [".c"]

linkKernel :: Config -> [FilePath] -> Build ()
linkKernel config objs = do
  ld kernelElf objs
  runProcess $ proc "objcopy" ["-I", "elf32-i386", "-O", "binary", kernelElf, kernelBin config]
  where
    kernelElf = kernelElfByConfig config

ld :: FilePath -> [FilePath] -> Build ()
ld output = runProcess . proc "ld.lld" . (["-T", kernelSrcDir </> "link.ld", "-o", output] ++)

kernelBin :: Config -> FilePath
kernelBin config = outdirByConfig Kernel config </> "kernel.bin"
