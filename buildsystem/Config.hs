module Config where

import System.FilePath

data Mod = Kernel | Userspace deriving (Eq)

data Mode = Dev | Release deriving (Eq)

data Config = Config {buildMode :: Mode, enableAsserts :: Bool}

instance Show Mod where
  show Kernel = "kernel"
  show Userspace = "userspace"

instance Show Mode where
  show Dev = "dev"
  show Release = "release"

kernelSrcDir :: FilePath
kernelSrcDir = "kernel"

userspaceSrcDir :: FilePath
userspaceSrcDir = "userspace"

userspacePrograms :: [String]
userspacePrograms = ["finite", "counter", "fibonacci", "factorial"]

outdir :: FilePath
outdir = "build"

outdirMod :: Mod -> FilePath
outdirMod m = outdir </> show m

outdirByConfig :: Mod -> Config -> FilePath
outdirByConfig m (Config mode asserts) =
  outdirMod m </> show mode </> (if asserts then "assert" else "noassert")

kernelElfByConfig :: Config -> FilePath
kernelElfByConfig config = outdirByConfig Kernel config </> "kernel.elf"

imgFileByConfig :: Config -> FilePath
imgFileByConfig config = outdirByConfig Kernel config </> "boot.img"

kernelSizeKb :: Integer
kernelSizeKb = 0x58400 `div` 1024

data RamSize = MiB Integer | GiB Integer

ramSize :: RamSize
ramSize = GiB 1

ramSizeForQemu :: RamSize -> String
ramSizeForQemu (GiB n) = show n ++ "G"
ramSizeForQemu (MiB n) = show n ++ "M"

ramSizeToBytes :: RamSize -> Integer
ramSizeToBytes (GiB n) = ramSizeToBytes (MiB (1024 * n))
ramSizeToBytes (MiB n) = 1024 * 1024 * n

constants :: [String]
constants =
  ("-DKERNEL_SIZE_KB=" ++ show kernelSizeKb)
    : ("-DRAM_SIZE=" ++ show (ramSizeToBytes ramSize))
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

clangFlags :: Mod -> Config -> [String]
clangFlags m (Config mode asserts) = prependIf "-DNDEBUG" flags (not asserts)
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
          Release -> "-Oz"
      ]
        ++ if m == Kernel then constants else []
    prependIf :: a -> [a] -> Bool -> [a]
    prependIf x xs True = x : xs
    prependIf _ xs False = xs

qemuFlags :: FilePath -> [String]
qemuFlags img =
  [ "-cpu",
    "pentium2",
    "-m",
    ramSizeForQemu ramSize,
    "-monitor",
    "stdio",
    "-device",
    "VGA",
    "-drive",
    "file=" ++ img ++ ",format=raw,if=floppy"
  ]
