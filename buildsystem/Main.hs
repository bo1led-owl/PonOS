module Main where

import Config
import Control.Exception (IOException, catch)
import Control.Monad.Extra
import Data.Functor
import Data.Time (UTCTime)
import Options.Applicative
import System.Directory
import System.FilePath
import Utils

clang :: Mode -> FilePath -> IO FilePath
clang mode input = do
  let output = ((`replaceDirectory` outdirByMode mode) . (-<.> "o")) input
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
buildC mode = getFilesWithExtensions srcdir [".c"] >>= traverse (clang mode)

buildKernel :: Mode -> IO FilePath
buildKernel mode = do
  loaderO <- buildLoader
  objs <- (loaderO :) <$> buildC mode
  ld kernelElf objs
  runProcess "objcopy" ["-I", "elf32-i386", "-O", "binary", kernelElf, kernelBin]
  pure kernelBin
  where
    kernelElf = outdirByMode mode </> "kernel.elf"
    kernelBin = outdirByMode mode </> "kernel.bin"
    ld :: FilePath -> [FilePath] -> IO ()
    ld output objs = runProcess "ld.lld" (["-e", "kernelEntry", "-T", "link.ld", "-o", output] <> objs)

shouldRebuild :: Mode -> IO Bool
shouldRebuild mode = do
  imgModTime <-
    catch
      (Just <$> getModificationTime imgFile)
      ((const $ pure Nothing) :: IOException -> IO (Maybe UTCTime))
  sourcesModTimes <- sources >>= traverse getModificationTime
  pure $ maybe True (\mt -> any (> mt) sourcesModTimes) imgModTime
  where
    imgFile = imgFileByMode mode

buildImg :: Mode -> IO FilePath
buildImg mode = do
  whenM
    (shouldRebuild mode)
    $ do
      createDirectoryIfMissing True (outdirByMode mode)
      kernelFile <- buildKernel mode
      checkSize kernelFile
      runProcess "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"]
      runProcess "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"]
  pure imgFile
  where
    checkSize kernelFile = do
      sz <- getFileSizeKb kernelFile
      when
        (sz >= kernelSizeKb || (kernelSizeKb - sz < 5))
        ( putStrLn $
            "\nWARNING: Kernel is close to upper limit: currently " ++ show sz ++ " KiB\n"
        )
    imgFile = imgFileByMode mode

run :: FilePath -> IO ()
run = runProcess "qemu-system-i386" . qemuFlags

debug :: FilePath -> IO ()
debug img = do
  runProcess' "qemu-system-i386" (qemuFlags img <> ["-s", "-S"])
  runProcess "lldb" ["--local-lldbinit"]

data Command
  = Build Mode
  | Run Mode
  | Debug Mode
  | Clean

buildMode :: Parser Mode
buildMode = flag Release Dev (long "dev" <> help "Build in dev mode")

args :: ParserInfo Command
args = info (parseCommands <**> helper) fullDesc
  where
    parseCommands =
      hsubparser $
        mconcat
          [ command "build" (info (Build <$> buildMode) (progDesc "Build kernel image")),
            command "run" (info (Run <$> buildMode) (progDesc "Run kernel")),
            command "debug" (info (Debug <$> buildMode) (progDesc "Debug kernel")),
            command "clean" (info (pure Clean) (progDesc "Clean all produced binaries"))
          ]

main :: IO ()
main = do
  cmd <- execParser args
  case cmd of
    Build m -> buildImg m $> ()
    Run m -> buildImg m >>= run
    Debug m -> buildImg m >>= debug
    Clean -> removeDirectoryRecursive outdir
