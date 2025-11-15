module Config where

import System.FilePath

data Mode = Dev | Release

data Config = Config {buildMode :: Mode, enableAsserts :: Bool}

instance Show Mode where
  show Dev = "dev"
  show Release = "release"

srcdir :: FilePath
srcdir = "src"

outdir :: FilePath
outdir = "build"

outdirByConfig :: Config -> FilePath
outdirByConfig (Config mode asserts) = outdir </> show mode </> (if asserts then "assert" else "noassert")

kernelSizeKb :: Integer
kernelSizeKb = 20

constants :: [String]
constants =
  ("-DKERNEL_SIZE_KB=" ++ show kernelSizeKb)
    : zipWith format names offsets
  where
    format name offset = "-D" ++ name ++ "=" ++ show offset
    names =
      [ "KERNEL_CODE_SEGMENT",
        "KERNEL_DATA_SEGMENT",
        "APP_CODE_SEGMENT",
        "APP_DATA_SEGMENT",
        "TSS_SEGMENT"
      ]
    offsets :: [Int]
    offsets = [i * 8 | i <- [1 ..]]

clangFlags :: Config -> [String]
clangFlags (Config mode asserts) = prependIf "-DNDEBUG" flags (not asserts)
  where
    flags =
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
        case mode of
          Dev -> "-O0"
          Release -> "-O2"
      ]
        ++ constants
    prependIf :: a -> [a] -> Bool -> [a]
    prependIf x xs True = x : xs
    prependIf _ xs False = xs

qemuFlags :: FilePath -> [String]
qemuFlags img =
  [ "-cpu",
    "pentium2",
    "-m",
    "4m",
    "-monitor",
    "stdio",
    "-device",
    "VGA",
    "-drive",
    "file=" ++ img ++ ",format=raw,if=floppy"
  ]

imgFileByConfig :: Config -> FilePath
imgFileByConfig config = outdirByConfig config </> "boot.img"
