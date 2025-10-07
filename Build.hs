{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Build (img, run, debug, dev, release, clean) where

import Control.Exception (IOException, catch)
import Control.Monad
import Control.Monad.Extra
import Data.Time (UTCTime)
import System.Directory
import System.FilePath
import System.IO
import System.Process (createProcess, proc, waitForProcess)
import System.Process qualified as SP (runProcess)

srcdir :: FilePath
srcdir = "src"

outdir :: FilePath
outdir = "build"

imgFile :: FilePath
imgFile = outdir </> "boot.img"

prevBuildModeFile :: FilePath
prevBuildModeFile = outdir </> "mode"

getPrevBuildMode :: IO (Maybe Mode)
getPrevBuildMode =
  ifM
    (doesFileExist prevBuildModeFile)
    (parseMode <$> readFile prevBuildModeFile)
    (pure Nothing)
  where
    parseMode "release" = Just Release
    parseMode "dev" = Just Dev
    parseMode _ = Nothing

sources :: IO [FilePath]
sources = listSourceFiles [".c", ".h", ".nasm"]

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

qemuFlags :: Args
qemuFlags =
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

data Mode = Dev | Release deriving (Eq)

instance Show Mode where
  show Dev = "dev"
  show Release = "release"

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

listCFiles :: IO [FilePath]
listCFiles = listSourceFiles [".c"]

listSourceFiles :: [String] -> IO [FilePath]
listSourceFiles extensions =
  map (srcdir </>)
    . filter (\f -> any (`isExtensionOf` f) extensions)
    <$> listDirectory srcdir

buildC :: Mode -> IO [FilePath]
buildC mode = listCFiles >>= traverse (clang mode)

getFileSizeKb :: FilePath -> IO Integer
getFileSizeKb path = withFile path ReadMode (fmap toKiBRoundedUp . hFileSize)
  where
    toKiBRoundedUp n = n `div` 1024 + (if n `mod` 1024 /= 0 then 1 else 0)

buildKernel :: Mode -> IO FilePath
buildKernel mode = do
  loaderO <- buildLoader
  objs <- (loaderO :) <$> buildC mode
  ld kernelElf objs
  runProcess "objcopy" ["-I", "elf32-i386", "-O", "binary", kernelElf, kernelBin]
  pure kernelBin
  where
    kernelElf = outdir </> "kernel.elf"
    kernelBin = outdir </> "kernel.bin"
    ld :: FilePath -> [FilePath] -> IO ()
    ld output objs = runProcess "ld.lld" (["-e", "kernelEntry", "-T", "link.ld", "-o", output] <> objs)

shouldRebuild :: Mode -> IO Bool
shouldRebuild mode = do
  imgModTime <-
    catch
      (Just <$> getModificationTime imgFile)
      ((const $ pure Nothing) :: IOException -> IO (Maybe UTCTime))
  sourcesModTimes <- sources >>= traverse getModificationTime
  let anySourceChanged = maybe True (\mt -> any (> mt) sourcesModTimes) imgModTime
  prevBuildMode <- getPrevBuildMode
  pure (anySourceChanged || maybe False (mode /=) prevBuildMode)

img :: Mode -> IO ()
img mode = ifM (shouldRebuild mode) (compile *> saveMode) (pure ())
  where
    checkSize kernelFile = do
      sz <- getFileSizeKb kernelFile
      when
        (sz >= kernelSizeKb || (kernelSizeKb - sz < 5))
        ( putStrLn $
            "\nWARNING: Kernel is close to upper limit: currently " ++ show sz ++ " KiB\n"
        )
    compile = do
      unlessM (doesDirectoryExist outdir) (createDirectory outdir)
      kernelFile <- buildKernel mode
      checkSize kernelFile
      runProcess "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"]
      runProcess "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"]
      pure imgFile
    saveMode = writeFile prevBuildModeFile (show mode)

dev :: IO ()
dev = img Dev

release :: IO ()
release = img Release

run :: IO ()
run = runProcess "qemu-system-i386" qemuFlags

debug :: IO ()
debug = do
  runProcess' "qemu-system-i386" (qemuFlags <> ["-s", "-S"])
  runProcess "lldb" ["--local-lldbinit"]

clean :: IO ()
clean = removeDirectoryRecursive outdir
