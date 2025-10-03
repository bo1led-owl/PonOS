{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Build (img, run, debug, dev, release) where

import Control.Monad
import Data.Traversable (for)
import System.Directory
import System.FilePath
import System.IO
import System.Process (createProcess, proc, waitForProcess)
import System.Process qualified as SP (runProcess)

srcdir :: FilePath
srcdir = "src"

outdir :: FilePath
outdir = "build"

kernelSizeKb :: Integer
kernelSizeKb = 20

clangFlags :: Args
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

qemuFlags :: FilePath -> Args
qemuFlags imgFile =
  [ "-cpu",
    "pentium2",
    "-m",
    "4g",
    "-monitor",
    "stdio",
    "-device",
    "VGA",
    "-drive",
    "file=" ++ imgFile ++ ",format=raw,if=floppy"
  ]

type Args = [String]

data Mode = Dev | Release

modeToFlags :: Mode -> Args
modeToFlags Dev = ["-O0"]
modeToFlags Release = ["-O2", "-DNDEBUG"]

runProcess :: FilePath -> Args -> IO ()
runProcess name args = do
  let process = proc name args
  (_, _, _, handle) <- createProcess process
  waitForProcess handle
  pure ()

runProcess' :: FilePath -> Args -> IO ()
runProcess' name args = do
  nullHandle <- Just <$> openFile "/dev/null" ReadWriteMode
  SP.runProcess name args Nothing Nothing nullHandle nullHandle nullHandle
  pure ()

clang :: Mode -> FilePath -> IO FilePath
clang mode input = do
  let output = ((`replaceDirectory` outdir) . (-<.> "o")) input
  putStrLn ("Building " ++ input ++ " to " ++ output)
  runProcess
    "clang"
    ( modeToFlags mode
        <> clangFlags
        <> [input, "-o", output]
    )
  pure output

buildLoader :: IO FilePath
buildLoader = do
  let input = srcdir </> "loader.nasm"
  let output = outdir </> "loader.o"
  runProcess "nasm" ["-felf32", "-dKERNEL_SIZE_KB=" ++ show kernelSizeKb, input, "-o", output]
  pure output

buildC :: Mode -> IO [FilePath]
buildC mode = do
  srcFiles <- map (srcdir </>) . filter (".c" `isExtensionOf`) <$> listDirectory srcdir
  for srcFiles (clang mode)

getFileSizeKb :: FilePath -> IO Integer
getFileSizeKb path = withFile path ReadMode (fmap toKiB . hFileSize)
  where
    toKiB n = n `div` 1024 + (if n `mod` 1024 /= 0 then 1 else 0)

buildKernel :: Mode -> IO FilePath
buildKernel mode = do
  let kernelElf = outdir </> "kernel.elf"
  let kernelBin = outdir </> "kernel.bin"
  loaderO <- buildLoader
  objs <- (loaderO :) <$> buildC mode
  ld kernelElf objs
  runProcess "objcopy" ["-I", "elf32-i386", "-O", "binary", kernelElf, kernelBin]
  pure kernelBin
  where
    ld :: FilePath -> [FilePath] -> IO ()
    ld output objs = runProcess "ld.lld" (["-e", "kernelEntry", "-T", "link.ld", "-o", output] <> objs)

img :: Mode -> IO FilePath
img mode = do
  doesDirectoryExist outdir >>= flip unless (createDirectory outdir)
  kernelFile <- buildKernel mode
  getFileSizeKb kernelFile
    >>= ( \sz ->
            when
              (sz >= kernelSizeKb || (kernelSizeKb - sz < 5))
              (putStrLn $ "\nWARNING: Kernel is close to upper limit: currently " ++ show sz ++ " KiB\n")
        )
  let imgFile = outdir </> "boot.img"
  runProcess "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"]
  runProcess "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"]
  pure imgFile

dev :: IO FilePath
dev = img Dev

release :: IO FilePath
release = img Release

run :: FilePath -> IO ()
run = runProcess "qemu-system-i386" . qemuFlags

debug :: FilePath -> IO ()
debug imgFile = do
  runProcess' "qemu-system-i386" (qemuFlags imgFile <> ["-s", "-S"])
  runProcess "lldb" ["--local-lldbinit"]

clean :: IO ()
clean = removeDirectoryRecursive outdir
