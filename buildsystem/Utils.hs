module Utils where

import Build
import Control.Exception (IOException, catch)
import Control.Monad.Except
import Control.Monad.Trans
import Data.Functor
import Data.Time
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Process hiding (runProcess)

prettyCmdSpec :: CmdSpec -> String
prettyCmdSpec (ShellCommand c) = c
prettyCmdSpec (RawCommand cmd args) = showCommandForUser cmd args

nullHandle :: IO Handle
nullHandle = openFile "/dev/null" ReadWriteMode

mute :: CreateProcess -> IO CreateProcess
mute p = do
  nh <- nullHandle
  pure $ p {std_in = UseHandle nh, std_out = UseHandle nh, std_err = UseHandle nh}

runProcess :: CreateProcess -> Build ()
runProcess p = do
  (_, _, _, handle) <- lift $ createProcess p
  exitCode <- lift $ waitForProcess handle
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure n -> throwError $ "Error: `" ++ prettyCmdSpec (cmdspec p) ++ "` exited with code " ++ show n

runProcessBackground :: CreateProcess -> Build ()
runProcessBackground p = (lift (mute p) >>= (lift . createProcess)) $> ()

runProcessSilent :: CreateProcess -> Build ()
runProcessSilent p = lift (mute p) >>= runProcess

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
