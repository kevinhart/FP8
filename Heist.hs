module Heist
( heist
, randomHeist
, randomHeistSeed
, randomHeistGen
) where

import System.Random

-- | @heist weights values max_weight@ returns the maximum value of the heist
heist :: (Num a, Num b) => [a] -> [b] -> a -> b
-- fake implementation
heist _ []     _ = 0
heist _ (b:bs) _ = b

-- Properties of the random tester
weightBounds :: (Num a, Random a) => (a, a)
weightBounds = (0, 10)
valueBounds :: (Num b, Random b) => (b, b)
valueBounds = (0, 100)
numItems = 100
maxWeight :: Int
maxWeight = 30

randomHeist :: (Num b, Random b) => IO b
randomHeist = do gen <- getStdGen
                 return $ randomHeistGen gen

randomHeistSeed :: (Num b, Random b) => Int -> b
randomHeistSeed seed = randomHeistGen (mkStdGen seed)

randomHeistGen :: (RandomGen g, Num b, Random b) => g -> b
randomHeistGen gen = heist weights values maxWeight where
  (weights, values) = unzip $ (take numItems) (itemPairs gen)

itemPairs :: (RandomGen g, Random a, Random b, Num a, Num b) => g -> [(a,b)]
itemPairs pgen = (w,v):(itemPairs pgen'') where
  (w, pgen')  = randomR weightBounds pgen
  (v, pgen'') = randomR  valueBounds pgen'
