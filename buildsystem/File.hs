module File where

import Target

newtype File = File FilePath

instance Target File where
  build (File _) = pure ()
  deps _ = pure NilDeps
  artifact (File f) = pure f
  needsRebuilding _ = pure False
  buildIfNeeded _ = pure ()
