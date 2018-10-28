{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Main where

import Criterion.Main
import Prelude

import qualified Data.Sequence as FT
import qualified Data.Sequence.FastCatQueue as CD
import qualified Data.SequenceClass as SC

import SList

pattern PNil <- (view -> VNil)
pattern PCons x xs <- (view -> VCons x xs)

-- List helper functions

l2slist :: [a] -> List a
l2slist = foldr cons nil

slist2l :: List a -> [a]
slist2l (view -> VNil) = []
slist2l (view -> VCons e es) = e:slist2l es

-- FT helper functions

ft2l :: FT.Seq a -> [a]
ft2l (SC.viewl -> SC.EmptyL) = []
ft2l (SC.viewl -> e SC.:< es) = e:ft2l es

-- CD helper functions

cd2l :: CD.FastTCQueue a -> [a]
cd2l (SC.viewl -> SC.EmptyL) = []
cd2l (SC.viewl -> e SC.:< es) = e:cd2l es

-- test revrev

revS :: List a -> List a
revS (view -> VNil) = nil
revS (view -> VCons e es) = revS es `app` cons e nil

test1S  = slist2l . revS . revS . l2slist

revFT :: FT.Seq a -> FT.Seq a
revFT (SC.viewl -> SC.EmptyL) = SC.empty
revFT (SC.viewl -> e SC.:< es) = revFT es SC.>< SC.singleton e

test1FT = ft2l . revFT . revFT . FT.fromList

revCD :: CD.FastTCQueue a -> CD.FastTCQueue a
revCD (SC.viewl -> SC.EmptyL) = SC.empty
revCD (SC.viewl -> e SC.:< es) = revCD es SC.>< SC.singleton e

test1CD = cd2l . revCD . revCD . foldr (SC.<|) SC.empty

test1 = ("revrev", [("SplayList", test1S), ("FingerTree", test1FT), ("CatDeque", test1CD)])

-- test sum

foldS :: b -> (a -> b -> b) -> List a -> b
foldS n c (view -> VNil) = n
foldS n c (view -> VCons e xs') = e `c` foldS n c xs'

test2E :: [Int] -> Int
test2E xs = foldE 0 (+) (l2slist xs)

test2S :: [Int] -> Int
test2S xs = foldS 0 (+) (l2slist xs)

test2 = ("sum", [("foldE", test2E), ("foldr", test2S)])

-- Benchmark helper functions

benchgroupNs (name, is) ns = [
         bgroup (name++"/"++show n) [
            bench imp (nf fun [1..n])
            | (imp, fun) <- is]
         | n <- ns ]

main :: IO ()
main = defaultMain ([]
                    ++ benchgroupNs test1 ([400000, 500000, 600000, 700000, 800000, 900000] :: [Int])
                    ++ benchgroupNs test2 ([4000000, 5000000, 6000000, 7000000, 8000000, 9000000] :: [Int])
                    )
