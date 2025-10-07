{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Utils where

import System.Directory
import System.FilePath
import System.IO
import System.Process (createProcess, proc, waitForProcess)
import System.Process qualified as SP (runProcess)

runProcess :: FilePath -> [String] -> IO ()
runProcess name args = do
  let process = proc name args
  (_, _, _, handle) <- createProcess process
  waitForProcess handle
  pure ()

runProcess' :: FilePath -> [String] -> IO ()
runProcess' name args = do
  nullHandle <- Just <$> openFile "/dev/null" ReadWriteMode
  SP.runProcess name args Nothing Nothing nullHandle nullHandle nullHandle
  pure ()

getFilesWithExtensions :: FilePath -> [String] -> IO [FilePath]
getFilesWithExtensions dir extensions =
  map (dir </>) . filter (\f -> any (`isExtensionOf` f) extensions)
    <$> listDirectory dir

getFileSizeKb :: FilePath -> IO Integer
getFileSizeKb path = withFile path ReadMode (fmap toKiBRoundedUp . hFileSize)
  where
    toKiBRoundedUp n = n `div` 1024 + (if n `mod` 1024 /= 0 then 1 else 0)
