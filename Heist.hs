module Heist
( heist
, randomHeist
, randomHeistSeed
, randomHeistGen
) where

import System.Random
import Data.Array

-- | @heist weights values max_weight@ returns the maximum value of the heist
heist :: (Ord b, Ix a, Enum a, Num a, Num b) => [a] -> [b] -> a -> b
heist ws vs maxw = table ! (len-1, maxw)
  where
    len = length ws
    table = array ((0, 0), (len-1, maxw))
               -- for (0,_) or (_,0) the maximum value is 0
              ([((0,x), 0) | x <- [0..maxw]] ++
               [((x,0), 0) | x <- [0..len-1]] ++
               [((i,j), cellvalue i j) | i <- [1..len-1], j <- [1..maxw]])
    cellvalue i j = let
                      weight = ws!!i
                      value = vs!!i
                    in
                      if weight > j
                        then
                          table!(i-1,j)
                        else
                          max (table!(i-1,j)) (table!(i-1,(j-weight)) + value)

-- Properties of the random tester
weightBounds :: (Num a, Random a) => (a, a)
weightBounds = (0, 10)
valueBounds :: (Num b, Random b) => (b, b)
valueBounds = (0, 100)
numItems = 100
maxWeight :: Int
maxWeight = 30

-- Doesn't update the global random number generator, so repeated calls will
-- return the same result until the global RNG gets changed.
randomHeist :: (Num b, Ord b, Random b) => IO b
randomHeist = do gen <- getStdGen
                 return $ randomHeistGen gen

randomHeistSeed :: (Num b, Ord b, Random b) => Int -> b
randomHeistSeed seed = randomHeistGen (mkStdGen seed)

randomHeistGen :: (RandomGen g, Num b, Ord b, Random b) => g -> b
randomHeistGen gen = heist weights values maxWeight where
  (weights, values) = unzip $ (take numItems) (itemPairs gen)

itemPairs :: (RandomGen g, Random a, Random b, Num a, Num b) => g -> [(a,b)]
itemPairs pgen = (w,v):(itemPairs pgen'') where
  (w, pgen')  = randomR weightBounds pgen
  (v, pgen'') = randomR  valueBounds pgen'
