{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Utils where

import Build
import Control.Exception (IOException, catch)
import Control.Monad.Except
import Control.Monad.Trans
import Data.Time
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process (createProcess, proc, waitForProcess)
import System.Process qualified as SP (runProcess)

runProcess :: FilePath -> [String] -> Build ()
runProcess name args = do
  let process = proc name args
  (_, _, _, handle) <- lift $ createProcess process
  exitCode <- lift $ waitForProcess handle
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure n -> throwError $ "Error: `" ++ name ++ "` exited with code " ++ show n

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

getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime file =
  catch
    (Just <$> getModificationTime file)
    ((const $ pure Nothing) :: IOException -> IO (Maybe UTCTime))

modifiedLaterThan :: FilePath -> UTCTime -> IO (Maybe Bool)
modifiedLaterThan a b = fmap (> b) <$> getModTime a
