{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Build where

import Config
import Control.Monad.Except
import Control.Monad.Trans
import Data.Kind

type Build = BuildT IO

newtype BuildT (m :: Type -> Type) a = BuildT {runBuildT :: Config -> m (Either String a)}

getConfig :: (Monad m) => BuildT m Config
getConfig = BuildT (pure . Right)

getFromConfig :: (Monad m) => (Config -> a) -> BuildT m a
getFromConfig f = BuildT (pure . Right . f)

instance (Functor f) => Functor (BuildT f) where
  fmap f m = BuildT (fmap (fmap f) . runBuildT m)

instance (Monad f) => Applicative (BuildT f) where
  pure x = BuildT (const $ (pure . Right) x)
  f <*> x = BuildT $ \c ->
    runBuildT f c
      >>= either
        (pure . Left)
        (\f -> fmap f <$> runBuildT x c)

instance (Monad m) => Monad (BuildT m) where
  m >>= g =
    BuildT $ \c -> do
      x <- runBuildT m c
      either (pure . Left) (\x -> runBuildT (g x) c) x

instance MonadTrans BuildT where
  lift m = BuildT $ const (Right <$> m)

instance (Monad m) => MonadError String (BuildT m) where
  throwError = BuildT . const . pure . Left
  catchError m h = BuildT $ \c -> do
    r <- runBuildT m c
    either (\err -> runBuildT (h err) c) (pure . Right) r
