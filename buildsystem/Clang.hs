{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Clang (Clang (..)) where

import Build
import Config
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Char
import Data.Void
import File
import System.Directory
import System.FilePath
import System.IO
import Target
import Text.Megaparsec
import Text.Megaparsec.Char
import Utils

newtype Clang = Clang FilePath

instance Target Clang where
  build c@(Clang file) = do
    config <- getConfig
    output <- artifact c
    lift $ putStrLn ("Building " ++ file ++ " to " ++ output)
    runProcess "clang" (clangFlags config ++ ["-c", file, "-o", output])
  deps (Clang input) = do
    let depFile = outdir </> "deps" </> replaceExtension input "d"
    unlessM
      (lift $ doesFileExist depFile)
      ( do
          lift $ createDirectoryIfMissing True (takeDirectory depFile)
          flags <- getFromConfig clangFlags
          runProcess "clang" (flags ++ ["-MM", "-MF", depFile, input])
      )
    depFileContents <- lift $ readFile' depFile
    pure (depsFromList File $ parseDepFile depFile depFileContents)
  artifact (Clang file) = getFromConfig (\c -> ((`replaceDirectory` outdirByConfig c) . (-<.> "o")) file)

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
