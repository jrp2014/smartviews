{-# LANGUAGE ViewPatterns, DeriveFunctor, LambdaCase #-}
module ML (ML, observeAllT, observeT, observeManyT) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Logic.Class
import Control.Monad.IO.Class
import Control.Applicative

newtype ML m a = ML { toView :: m (Maybe (a, ML m a)) } deriving (Functor)

fromView = ML

single a = return (Just (a,mzero))

instance Monad m => Applicative (ML m) where
  pure = fromView . single
  (<*>) = ap


instance Monad m => Monad (ML m) where
  return = pure
  (toView -> m) >>= f = fromView $ m >>= \case 
       Nothing    -> return Nothing
       Just (h,t) -> toView (f h `mplus` (t >>= f))
  fail _ = mzero

instance Monad m => MonadPlus (ML m) where
  mzero = fromView (return Nothing)
  mplus (toView -> a) b = fromView $ a >>= \case
     Nothing    -> toView b
     Just (h,t) -> return (Just (h,t `mplus` b))

instance Monad m => Alternative (ML m) where
  empty = mzero
  (<|>) = mplus

instance MonadTrans ML where
  lift m = fromView (m >>= single)

instance Monad m => MonadLogic (ML m) where
  msplit (toView -> m) = lift m

observeAllT :: Monad m => ML m a -> m [a]
observeAllT (toView -> m) = m >>= get where
      get (Just (a,t)) = fmap (a :) (observeAllT t)
      get _            = return []

observeT :: Monad m => ML m a -> m a
observeT (toView -> m) = m >>= get where
      get (Just (a,t)) = return a
      get _            = fail "No results"

observeManyT :: Monad m => Int -> ML m a -> m [a]
observeManyT n _ | n <= 0 = return []
observeManyT n (toView -> m) = m >>= get where
  get (Just (a, t)) = observeManyT (n-1) t >>= \xs -> return (a:xs)
  get Nothing = return []

instance (MonadIO m) => MonadIO (ML m) where
    liftIO = lift . liftIO

