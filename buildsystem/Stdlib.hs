{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Stdlib (Stdlib (..)) where

import Build
import Clang
import Config
import NASM
import System.FilePath
import System.Process (proc)
import Target
import Utils

data Stdlib = Stdlib

instance Target Stdlib where
  build Stdlib = do
    cO <- artifact c
    nasmO <- artifact nasm
    art <- artifact Stdlib
    runProcess $ proc "ld.lld" ["--relocatable", cO, nasmO, "-o", art]
  deps Stdlib = pure (nasm :> c :> NilDeps)
  artifact Stdlib = getFromConfig (\c -> outdirByConfig Userspace c </> "lib.o")

nasm :: NASM
nasm = NASM Userspace (userspaceSrcDir </> "lib.nasm")

c :: Clang
c = Clang Userspace (userspaceSrcDir </> "lib.c")
