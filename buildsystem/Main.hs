{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Build
import Config
import Control.Monad.Trans
import Data.Functor
import Data.Tuple.Extra
import Image
import Options.Applicative
import System.Directory
import Target
import Utils

data Command
  = Build Config
  | Run Config
  | Debug Config
  | Clean

args :: ParserInfo Command
args = info (parseCommands <**> helper) fullDesc
  where
    config =
      Config
        <$> flag Release Dev (short 'd' <> long "dev" <> help "Build without optimizations")
        <*> flag True False (short 'n' <> long "noassert" <> help "Disable assertions")
    parseCommands =
      (hsubparser . mconcat . map (uncurry3 $ \n c d -> command n (info c (progDesc d))))
        [ ("build", Build <$> config, "Build kernel image"),
          ("run", Run <$> config, "Run kernel"),
          ("debug", Debug <$> config, "Debug kernel"),
          ("clean", pure Clean, "Clean all produced binaries")
        ]

main :: IO ()
main = do
  cmd <- execParser args
  res <- case cmd of
    Build c -> runBuildT buildImage c
    Run c -> runBuildT (buildImage *> run) c
    Debug c -> runBuildT (buildImage *> debug) c
    Clean -> removeDirectoryRecursive outdir $> Right ()
  either putStrLn pure res

buildImage :: Build ()
buildImage = runTarget Image

run :: Build ()
run = do
  img <- getFromConfig imgFileByConfig
  runProcess "qemu-system-i386" (qemuFlags img)

debug :: Build ()
debug = do
  img <- getFromConfig imgFileByConfig
  lift $ runProcessBackground "qemu-system-i386" (qemuFlags img ++ ["-s", "-S"])
  runProcess "lldb" ["--local-lldbinit"]
