module UserspaceProgram (UserspaceProgram (..)) where

import Build
import Clang
import Config
import NASM
import Stdlib
import System.FilePath
import System.Process (proc)
import Target
import Utils

newtype UserspaceProgram = UserspaceProgram String

clang :: FilePath -> Clang
clang = Clang Userspace

src :: String -> FilePath
src n = userspaceSrcDir </> (n <.> "c")

bin :: String -> Config -> FilePath
bin n config = outdirByConfig Userspace config </> (n <.> "bin")

elf :: String -> Config -> FilePath
elf n config = outdirByConfig Userspace config </> (n <.> "elf")

instance Target UserspaceProgram where
  build (UserspaceProgram n) = do
    config <- getConfig
    startupO <- artifact startup
    obj <- artifact (clang (src n))
    stdlib <- artifact Stdlib
    linkProgram n config [startupO, stdlib, obj]
  deps (UserspaceProgram n) = pure (startup :> clang (src n) :> Stdlib :> NilDeps)
  artifact (UserspaceProgram n) = getFromConfig (bin n)
  name (UserspaceProgram n) = Just n

startup :: NASM
startup = NASM Userspace (userspaceSrcDir </> "startup.nasm")

linkProgram :: String -> Config -> [FilePath] -> Build ()
linkProgram n config objs = do
  ld e objs
  runProcess $ proc "objcopy" ["-I", "elf32-i386", "-O", "binary", e, b]
  where
    e = elf n config
    b = bin n config

ld :: FilePath -> [FilePath] -> Build ()
ld output = runProcess . proc "ld.lld" . (["-T", userspaceSrcDir </> "link.ld", "-o", output] ++)
