{-# LANGUAGE DeriveFunctor,
             FlexibleInstances,
             FlexibleContexts,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables
             #-}

module Main where

import Control.Monad
import Criterion.Main
import Test.QuickCheck

import Control.Monad.Free(MonadFree, wrap)
import qualified Control.Monad.Free as F
import qualified Control.Monad.Codensity as C
import qualified Control.Monad.Operational as O
import qualified Control.Monad.Free.Reflectable as FR
import qualified Control.Monad.Operational.Reflectable as OR

import Control.Monad.Identity

import qualified SFree as SF

-- MonadFree instances that are not present in Operational
--  and Reflectable instances.

instance Functor f => MonadFree f (O.Program f) where
  wrap v = O.singleton v >>= id

instance Functor f => MonadFree f (FR.FreeMonad f) where
  wrap v = FR.fromView (FR.Impure v)

instance Functor f => MonadFree f (OR.Program f) where
  wrap v = OR.fromView (OR.Bind v id)

-- General views

data View m f x = Var x | Con (f (m x))

class MonadFree f m => Reflective f m where
  view :: m x -> View m f x

instance Functor f => Reflective f (SF.Free f) where
  view m = case SF.toView m of
    SF.Var x -> Var x
    SF.Con v -> Con v

instance Functor f => Reflective f (F.Free f) where
  view (F.Pure x) = Var x
  view (F.Free v) = Con v

instance Functor f => Reflective f (O.Program f) where
  view m = case O.view m of
    O.Return x -> Var x
    v O.:>>= c -> Con (fmap c v)

instance Functor f => Reflective f (FR.FreeMonad f) where
  view m = case FR.toView m of
    FR.Pure x -> Var x
    FR.Impure v -> Con v

instance Functor f => Reflective f (OR.Program f) where
  view m = case OR.toView m of
    OR.Return x -> Var x
    OR.Bind v c -> Con (fmap c v)

-- Voigtlander's example

data Node x = Node x x deriving Functor

fullTree :: MonadFree Node m => Int -> m Int
fullTree 1 = return 1
fullTree n = do
  i <- fullTree (n - 1)
  wrap (Node (return (n - 1 - i)) (return (i + 1)))

zigzag :: Reflective Node m => m Int -> Int
zigzag = zig
  where zig x = case view x of
          Var n -> n
          (Con (Node t1 t2)) -> zag t1
        zag x = case view x of
          Var n -> n
          (Con (Node t1 t2)) -> zig t2

-- Reflection without remorse's example

data Get a = Get (Int -> a) deriving Functor

get :: MonadFree Get m => m Int
get = wrap (Get (\x -> return x))

addGet :: MonadFree Get m => Int -> m Int
addGet x = liftM (+ x) get

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

addNbad :: MonadFree Get m => Int -> m Int
addNbad n = foldl (>>>) return (replicate n addGet) 0

feedAll :: Reflective Get m => m b -> [Int] -> Maybe b
feedAll v xs = case view v of
  Var a -> Just a
  Con (Get f) -> case xs of
    [] -> Nothing
    (h : t) -> feedAll (f h) t

-- Interleaving

interleave :: (Functor f, Reflective f m) => m a -> m b -> m (a, b)
interleave v v' = case view v of
  Var a -> liftM (\b -> (a, b)) v'
  Con v'' -> wrap (fmap (\v -> liftM sw (interleave v' v)) v'')
    where sw (x,y) = (y, x)


-- test fulltree

test1S :: Int -> Int
test1S n = zigzag (fullTree n :: SF.Free Node Int)

test1Oper :: Int -> Int
test1Oper n = zigzag (fullTree n :: O.Program Node Int)

test1Ref :: Int -> Int
test1Ref n = zigzag (fullTree n :: FR.FreeMonad Node Int)

test1Codensity :: Int -> Int
test1Codensity n = zigzag (C.improve (fullTree n))

test1 = ("monad-fulltree", [("SplayFree", test1S), ("Oper",test1Oper), ("Ref", test1Ref), ("Codensity",test1Codensity)])

-- test interleave

test2S :: Int -> Maybe (Int, Int)
test2S n =  feedAll (interleave (addNbad n) (addNbad n) :: SF.Free Get (Int, Int)) [1..n]

--intcod :: Int -> Maybe (Int, Int)
--intcod n =  feedAll (interleave (C.improve (addNbad n)) (C.improve (addNbad n))) [1..n]

test2Ref :: Int -> Maybe (Int, Int)
test2Ref n =  feedAll (interleave (addNbad n) (addNbad n) :: FR.FreeMonad Get (Int, Int)) [1..n]

test2Oper :: Int -> Maybe (Int, Int)
test2Oper n =  feedAll (interleave (addNbad n) (addNbad n) :: O.Program Get (Int, Int)) [1..n]

test2 = ("monad-interleave", [("SplayFree", test2S), ("Ref", test2Ref), ("Oper", test2Oper)])

benchgroupNs (name, is) ns = [
         bgroup (name++"/"++show n) [
            bench imp (nf fun n)
            | (imp, fun) <- is]
         | n <- ns ]

main = defaultMain ([]
  ++ benchgroupNs  test1 [300000,400000,500000,600000,700000,800000,900000]
  ++ benchgroupNs  test2 [300000,400000,500000,600000,700000,800000,900000]
  )
