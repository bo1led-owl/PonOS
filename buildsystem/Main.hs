module Main where

import Config
import Control.Exception (IOException, catch)
import Control.Monad.Extra
import Data.Functor
import Data.Time (UTCTime)
import Data.Tuple.Extra
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
        ++ clangFlags
        ++ [input, "-o", output]
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
    ld output objs = runProcess "ld.lld" (["-e", "kernelEntry", "-T", "link.ld", "-o", output] ++ objs)

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

buildImg :: Mode -> IO (Either String FilePath)
buildImg mode = ifM (shouldRebuild mode) makeImg (pure $ Right imgFile)
  where
    makeImg = do
      createDirectoryIfMissing True (outdirByMode mode)
      kernelFile <- buildKernel mode
      ifM
        (checkSize kernelFile)
        (dd kernelFile $> Right imgFile)
        (pure $ Left "Kernel is too big, aborting compilation")
    dd kernelFile = do
      runProcess "dd" ["if=/dev/zero", "of=" ++ imgFile, "bs=1024", "count=1440"]
      runProcess "dd" ["if=" ++ kernelFile, "of=" ++ imgFile, "conv=notrunc"]
    checkSize = fmap (< kernelSizeKb) . getFileSizeKb
    imgFile = imgFileByMode mode

data Command
  = Build Mode
  | Run Mode
  | Debug Mode
  | Clean

args :: ParserInfo Command
args = info (parseCommands <**> helper) fullDesc
  where
    buildMode = flag Release Dev (long "dev" <> help "Build in dev mode")
    parseCommands =
      (hsubparser . mconcat . map (uncurry3 $ \n c d -> command n (info c (progDesc d))))
        [ ("build", Build <$> buildMode, "Build kernel image"),
          ("run", Run <$> buildMode, "Run kernel"),
          ("debug", Debug <$> buildMode, "Debug kernel"),
          ("clean", pure Clean, "Clean all produced binaries")
        ]

main :: IO ()
main = do
  cmd <- execParser args
  case cmd of
    Build m -> runIfBuildSucceedes (const $ pure ()) m
    Run m -> run m
    Debug m -> debug m
    Clean -> removeDirectoryRecursive outdir

runIfBuildSucceedes :: (FilePath -> IO ()) -> Mode -> IO ()
runIfBuildSucceedes f m = buildImg m >>= either putStrLn f

run :: Mode -> IO ()
run = runIfBuildSucceedes $ runProcess "qemu-system-i386" . qemuFlags

debug :: Mode -> IO ()
debug = runIfBuildSucceedes $ \img -> do
  runProcess' "qemu-system-i386" (qemuFlags img ++ ["-s", "-S"])
  runProcess "lldb" ["--local-lldbinit"]
