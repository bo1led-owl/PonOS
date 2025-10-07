module Config where

import System.FilePath
import Utils

data Mode = Dev | Release deriving (Eq)

instance Show Mode where
  show Dev = "dev"
  show Release = "release"

modeToFlags :: Mode -> [String]
modeToFlags Dev = ["-O0"]
modeToFlags Release = ["-O2", "-DNDEBUG"]

srcdir :: FilePath
srcdir = "src"

outdir :: FilePath
outdir = "build"

outdirByMode :: Mode -> FilePath
outdirByMode mode = outdir </> show mode

kernelSizeKb :: Integer
kernelSizeKb = 20

clangFlags :: [String]
clangFlags =
  [ "-Wall",
    "-Wextra",
    "-Wpedantic",
    "-masm=intel",
    "-std=c23",
    "-m32",
    "-ffreestanding",
    "-fno-pie",
    "-mno-sse",
    "-mno-mmx",
    "-fno-stack-protector",
    "-DKERNEL_SIZE_KB=" ++ show kernelSizeKb,
    "-c"
  ]

qemuFlags :: FilePath -> [String]
qemuFlags img =
  [ "-cpu",
    "pentium2",
    "-m",
    "4g",
    "-monitor",
    "stdio",
    "-device",
    "VGA",
    "-drive",
    "file=" ++ img ++ ",format=raw,if=floppy"
  ]

imgFileByMode :: Mode -> FilePath
imgFileByMode mode = outdirByMode mode </> "boot.img"

sources :: IO [FilePath]
sources = getFilesWithExtensions srcdir [".c", ".h", ".nasm"]
