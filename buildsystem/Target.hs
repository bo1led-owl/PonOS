{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Target where

import Build
import Config
import Control.Monad.Extra
import Control.Monad.Reader
import Data.Functor
import Data.Kind
import System.Directory
import System.FilePath
import Utils

data Deps where
  NilDeps :: Deps
  (:>) :: (Target t) => t -> Deps -> Deps

infixr 5 :>

depsFromList :: (Target t) => (a -> t) -> [a] -> Deps
depsFromList _ [] = NilDeps
depsFromList f (x : xs) = f x :> depsFromList f xs

mapDeps :: () => (forall t. (Target t) => t -> a) -> Deps -> [a]
mapDeps _ NilDeps = []
mapDeps f (d :> ds) = f d : mapDeps f ds

traverseDeps :: (Applicative f) => (forall t. (Target t) => t -> f b) -> Deps -> f [b]
traverseDeps _ NilDeps = pure []
traverseDeps f (d :> ds) = (:) <$> f d <*> traverseDeps f ds

runTarget :: (Target t) => t -> Build ()
runTarget = buildIfNeeded

getArtifact :: (Target t) => t -> Config -> IO (Either String FilePath)
getArtifact t = runBuildT (artifact t)

class Target (t :: Type) where
  {-# MINIMAL build, deps, artifact #-}

  build :: t -> Build ()
  deps :: () => t -> Build Deps
  artifact :: () => t -> Build FilePath

  name :: t -> Maybe String
  name _ = Nothing

  buildIfNeeded :: () => t -> Build ()
  buildIfNeeded t =
    whenM
      (needsRebuilding t)
      $ do
        prepare t
        deps t >>= buildAllDeps
        lift $ case name t of
          Just name -> putStrLn ("Building " <> name)
          Nothing -> pure ()
        build t

  prepare :: t -> Build ()
  prepare t = do
    artifactDir <- takeDirectory <$> artifact t
    lift $ createDirectoryIfMissing True artifactDir

  needsRebuilding :: t -> Build Bool
  needsRebuilding t = do
    artifactModTime <- artifact t >>= lift . getModTime
    case artifactModTime of
      Just artifactModTime -> do
        deps <- deps t
        depArtifacts <- traverseDeps artifact deps
        someArtifactIsOld <-
          anyM
            (fromMaybeM (pure True) . (lift . (`modifiedLaterThan` artifactModTime)))
            depArtifacts
        someDepIsOld <- fmap or (traverseDeps needsRebuilding deps)
        pure $ someArtifactIsOld || someDepIsOld
      Nothing -> pure True

buildAllDeps :: Deps -> Build ()
buildAllDeps d = traverseDeps buildIfNeeded d $> ()
