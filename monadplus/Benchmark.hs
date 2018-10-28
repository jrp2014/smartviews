{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module Main where

import qualified Fixed.Logic as Ref
import qualified SFMP
import qualified Control.Monad.Logic as Lo
import qualified SListT
import qualified ML

import Control.Monad
import Control.Monad.Free(MonadFree, wrap)
import Control.Monad.Logic.Class
import Control.Monad.Identity
import Criterion.Main

data Implementations = Ref
                     | SFMP
                     | Lo
                     | ML
                     | ListT
                     deriving (Show,Enum)

observeAllTs :: Monad m => (forall m. MonadLogic m => Int -> m a)
                -> [Int -> m [a]]
observeAllTs f = [
                Ref.observeAllT . f
              , SFMP.observeAllT . f
              , Lo.observeAllT . f
              , ML.observeAllT . f
              , SListT.observeAllT . f
              ]

observeTs :: Monad m => (forall m. MonadLogic m => Int -> m [Int])
             -> [Int -> m [Int]]
observeTs f = [
                Ref.observeT . f
              , SFMP.observeT . f
              , Lo.observeT . f
              , ML.observeT . f
              , SListT.observeT . f
            ]

-- Benchmark 1 : interleave

mchoose :: MonadPlus m => [a] -> m a
mchoose l = foldr mplus mzero $ map return l

bench1 :: MonadLogic m => Int -> m Int
bench1 n = mchoose [1..n] `interleave` mchoose [n,n-1..1]

bench1s :: Monad m => (String,[Int -> m [Int]])
bench1s = ("interleave", observeAllTs bench1)

-- Benchmark 2 : take n nats

seqN :: MonadLogic m => Int -> m a -> m [a]
seqN n m | n == 0     = return []
         | otherwise  = msplit m >>= \x -> case x of
                          Nothing    -> return []
                          Just (a,m) -> liftM (a:) $ seqN (n-1) m

nats :: MonadPlus m => m Int
nats = natsFrom 0 where
  natsFrom n = return n `mplus` natsFrom (n + 1)

bench2 :: MonadLogic m => Int -> m [Int]
bench2 n = seqN n nats

bench2s :: Monad m => (String,[Int -> m [Int]])
bench2s = ("Take-N", observeTs bench2)

-- Benchmark 3 : left skewed mplus

bench3 :: MonadPlus m => Int -> m Int
bench3 n = foldl mplus mzero $ replicate n (return 1)

bench3s :: Monad m => (String,[Int -> m [Int]])
bench3s = ("left-mplus",observeAllTs bench3)


--------------------------------------------------

benchgroupNs (name,bs) is ns = [
         bgroup (name++"/"++show n) [
    bench (show impl) (nf (runIdentity . (bs !! fromEnum impl)) n) | impl <- is]
               | n <- ns ]



main = defaultMain ( []
  ++ benchgroupNs  bench1s [SFMP, Ref, ListT] [100000,200000,300000,400000]
  ++ benchgroupNs  bench2s [ListT, SFMP, Ref, ML] [100000, 200000, 300000, 400000]
  ++ benchgroupNs bench3s [ListT, SFMP, Ref, Lo] [500000, 600000, 700000, 800000]
                   )

