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
import System.Process qualified as SP (runProcess, waitForProcess)

runProcess :: FilePath -> [String] -> Build ()
runProcess name args = do
  handle <- lift $ SP.runProcess name args Nothing Nothing Nothing Nothing Nothing
  exitCode <- lift $ SP.waitForProcess handle
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure n -> throwError $ "Error: `" ++ name ++ "` exited with code " ++ show n

runProcessSilent :: FilePath -> [String] -> Build ()
runProcessSilent name args = do
  out <- lift $ Just <$> nullHandle
  handle <- lift $ SP.runProcess name args Nothing Nothing Nothing out out
  exitCode <- lift $ SP.waitForProcess handle
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure n -> throwError $ "Error: `" ++ name ++ "` exited with code " ++ show n

runProcessBackground :: FilePath -> [String] -> IO ()
runProcessBackground name args = do
  nh <- Just <$> nullHandle
  SP.runProcess name args Nothing Nothing nh nh nh
  pure ()

nullHandle :: IO Handle
nullHandle = openFile "/dev/null" ReadWriteMode

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
