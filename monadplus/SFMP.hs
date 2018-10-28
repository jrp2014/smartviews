{-# LANGUAGE ExistentialQuantification, ViewPatterns,
  FlexibleInstances #-}

module SFMP
  ( FMP
  , singleton
  , FFMP(..)
  , con
  , lift
  , MonadPlusView(..)
  , monadPlusView
  , observeAllT
  , observeT
  , observeManyT
  , module Control.Monad
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Logic.Class
import Control.Monad.Trans hiding (lift)

data FMP f x
  = FZero
  | FCons (FFMP f x)
          (FMP f x)
  | (FMP f x) :+ (FMP f x)
  | forall a. (FMP f a) :>>= (a -> FMP f x)

data FFMP f x
  = FVar x
  | FCon (f (FMP f x))

instance Show (FFMP Identity x) where
  show (FVar x) = "(FVar _)"
  show (FCon (Identity v)) = "(FCon " ++ show v ++ ")"

instance Show (FMP Identity x) where
  show FZero = "FZero"
  show (m :+ n) = "(" ++ show m ++ " :+ " ++ show n ++ ")"
  show (FCons v m) = "(" ++ "FCons " ++ show v ++ " " ++ show m ++ ")"
  show (m :>>= f) = "(" ++ show m ++ ":>>=)"

singleton :: FFMP f x -> FMP f x
singleton f = FCons f FZero

instance Functor (FMP f) where
  fmap = liftM

instance Applicative (FMP f) where
  pure x = singleton (FVar x)
  (<*>) = ap

instance Monad (FMP f) where
  return x = singleton (FVar x)
  m >>= f = m :>>= f

instance MonadPlus (FMP f) where
  mzero = FZero
  x `mplus` y = x :+ y

instance Alternative (FMP f) where
  empty = mzero
  (<|>) = mplus

con :: f (FMP f x) -> FMP f x
con = singleton . FCon

lift :: Monad f => f x -> FMP f x
lift x = con (return <$> x)

data MonadPlusView f x
  = MZero
  | MCons (FFMP f x)
          (FMP f x)

monadPlusView :: Monad f => FMP f x -> MonadPlusView f x
monadPlusView FZero = MZero
monadPlusView (FCons x xs) = MCons x xs
monadPlusView ((x :+ y) :+ z) = monadPlusView (x :+ (y :+ z)) -- mplus assoc
monadPlusView (x :+ y) =
  case monadPlusView x of
    MZero -> monadPlusView y
    MCons x xs -> MCons x (xs :+ y)
monadPlusView ((m :>>= f) :>>= g) = monadPlusView (m :>>= (\x -> f x :>>= g)) -- bind assoc
--monadPlusView ((x :+ y) :>>= f)    = monadPlusView ((x :>>= f) :+ (y :>>= f)) -- left distrib
monadPlusView (m :>>= f) =
  case monadPlusView m of
    MZero -> MZero
    MCons (FVar x) xs -> monadPlusView (f x :+ (xs :>>= f))
    MCons (FCon t) xs -> MCons (FCon ((:>>= f) <$> t)) (xs :>>= f)

--------------------------------------------------
mpv :: Monad f => FMP f x -> MonadPlusView f x
mpv = monadPlusView

instance Monad m => MonadLogic (FMP m) where
  msplit x =
    case mpv x of
      MZero -> return Nothing
      MCons (FVar a) xs -> return (Just (a, xs))
      MCons (FCon m) xs -> FCons (FCon (msplit <$> m)) (msplit xs)

observeAllT :: (Monad m) => FMP m a -> m [a]
observeAllT (mpv -> MZero) = return []
observeAllT (mpv -> MCons (FVar v) xs) = fmap (v :) (observeAllT xs)
observeAllT (mpv -> MCons (FCon m) xs) = do
  as <- m >>= observeAllT
  bs <- observeAllT xs
  return (as ++ bs)

observeT :: (Monad m) => FMP m a -> m a
observeT (mpv -> MCons (FVar v) _) = return v
observeT (mpv -> MCons (FCon m) _) = m >>= observeT

observeManyT :: Monad m => Int -> FMP m a -> m [a]
observeManyT n _
  | n <= 0 = return []
observeManyT n (mpv -> MZero) = return []
observeManyT n (mpv -> MCons (FVar v) xs) = do
  xs' <- observeManyT (n - 1) xs
  return (v : xs')
observeManyT n (mpv -> MCons (FCon m) xs) = do
  m' <- m
  m'' <- observeManyT n m'
  xs' <- observeManyT (n - length m'') xs
  return (m'' ++ xs')

instance (MonadIO m) => MonadIO (FMP m) where
  liftIO = SFMP.lift . liftIO
