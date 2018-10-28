{-# LANGUAGE ExistentialQuantification, ViewPatterns, LambdaCase
  #-}

module SListT
  ( ListT
  , observeT
  , observeAllT
  , observeManyT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Logic.Class
import Control.Monad.Trans

data ListT m a
  = Con (m (Maybe (a, ListT m a)))
  | (ListT m a) :+ (ListT m a)

instance Monad m => Functor (ListT m) where
  fmap = liftM

instance Monad m => Applicative (ListT m) where
  pure x = Con (return (Just (x, mzero)))
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = pure
  m >>= f =
    Con $
    view m >>= \case
      Nothing -> return Nothing
      Just (h, t) -> view (f h `mplus` (t >>= f))

instance Monad m => MonadPlus (ListT m) where
  mzero = Con (return Nothing)
  mplus = (:+)

instance Monad m => Alternative (ListT m) where
  empty = mzero
  (<|>) = mplus

type View m a = m (Maybe (a, ListT m a))

view :: Monad m => ListT m a -> View m a
view (Con v) = v
view ((m :+ n) :+ o) = view (m :+ (n :+ o))
view (m :+ n) =
  view m >>= \case
    Nothing -> view n
    Just (h, t) -> return (Just (h, t `mplus` n))

instance MonadTrans ListT where
  lift v = Con (v >>= \x -> return (Just (x, mzero)))

instance Monad m => MonadLogic (ListT m) where
  msplit v = lift (view v)

observeAllT :: Monad m => ListT m a -> m [a]
observeAllT (view -> m) = m >>= get
  where
    get (Just (a, t)) = fmap (a :) (observeAllT t)
    get _ = return []

observeT :: Monad m => ListT m a -> m a
observeT (view -> m) = m >>= get
  where
    get (Just (a, t)) = return a
    get _ = fail "No results"

observeManyT :: Monad m => Int -> ListT m a -> m [a]
observeManyT n _
  | n <= 0 = return []
observeManyT n (view -> m) = m >>= get
  where
    get (Just (a, t)) = observeManyT (n - 1) t >>= \xs -> return (a : xs)
    get Nothing = return []

instance (MonadIO m) => MonadIO (ListT m) where
  liftIO = lift . liftIO
