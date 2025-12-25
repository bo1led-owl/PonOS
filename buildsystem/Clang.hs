{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Clang (Clang (..)) where

import Build
import Config
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Char
import Data.Time (UTCTime)
import Data.Void
import File
import System.Directory
import System.FilePath
import System.IO
import System.Process (proc)
import Target
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

data Clang = Clang Mod FilePath

instance Target Clang where
  build c@(Clang m file) = do
    config <- getConfig
    output <- artifact c
    runProcess $ proc "clang" (clangFlags m config ++ ["-c", file, "-o", output])
  deps c@(Clang m input) = do
    artifactModTime <- artifact c >>= lift . getModTime
    getDeps m input artifactModTime
  artifact (Clang m file) = getFromConfig (\c -> ((`replaceDirectory` outdirByConfig m c) . (<.> "o")) file)
  name (Clang _ file) = Just file

getDeps :: Mod -> FilePath -> Maybe UTCTime -> BuildT IO Deps
getDeps m input artifactModTime = do
  let depFile = outdirMod m </> "deps" </> replaceExtension input "d"
  depFileModTime <- lift $ getModTime depFile
  let depFileIsUpToDate = maybe False (uncurry (>=)) ((,) <$> depFileModTime <*> artifactModTime)
  unless
    depFileIsUpToDate
    ( do
        lift $ createDirectoryIfMissing True (takeDirectory depFile)
        flags <- getFromConfig (clangFlags m)
        runProcess $ proc "clang" (flags ++ ["-MM", "-MF", depFile, input])
    )
  depFileContents <- lift $ readFile' depFile
  pure (depsFromList File $ parseDepFile depFile depFileContents)

parseDepFile :: FilePath -> String -> [FilePath]
parseDepFile f c = either (error . errorBundlePretty) id (runParser p f c)

p :: Parsec Void String [FilePath]
p = do
  many (anySingleBut ':')
  char ':'
  space
  res <- takeWhile1P (Just "dependency") (not . isSpace) `sepEndBy` many (char '\\' <|> spaceChar)
  eof
  pure res
