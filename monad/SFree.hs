 {-# LANGUAGE ExistentialQuantification, FlexibleInstances,
    MultiParamTypeClasses #-}


module SFree (Free, FreeMonadView(..), toView) where

import Control.Monad
import Control.Monad.Free hiding (Free)

-- Monad

data Free f x = FVar x | FCon (f (Free f x)) | forall a . (Free f a) :>>= (a -> Free f x)

instance Functor (Free f) where
  fmap = liftM

instance Applicative (Free f) where
  pure = FVar
  (<*>) = ap

instance Monad (Free f) where
  return = pure
  m >>= f = m :>>= f

instance MonadFree f (Free f) where
  wrap = FCon

lift :: Functor f => f x -> Free f x
lift x = FCon (fmap return x)

primrec :: Functor f => (z -> w) -> (f (w, Free f z) -> w) -> Free f z -> w
primrec c r (FVar x)             = c x
primrec c r (FCon v)             = r (fmap (\z -> (primrec c r z, z)) v)
primrec c r ((m :>>= f) :>>= f') = primrec c r (m :>>= \x -> f x :>>= f')
primrec c r (m :>>= f)           = primrec (primrec c r . f) r' m
  where r' v = r (fmap (\(v,n) -> (v, n :>>= f)) v)

-- FreeMonadViews

data FreeMonadView f x = Var x | Con (f (Free f x))

toView :: Functor f => Free f x -> FreeMonadView f x
toView (FVar x)             = Var x
toView (FCon v)             = Con v
toView ((m :>>= f) :>>= f') = toView (m :>>= \x -> f x :>>= f')
toView (m :>>= f)           = case toView m of
    Var x  -> toView (f x)
    Con v  -> Con (fmap (:>>= f) v)
