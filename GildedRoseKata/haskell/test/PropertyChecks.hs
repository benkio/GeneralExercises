module PropertyChecks where

import Test.QuickCheck
import GildedRose
import Generators

qualityCheckSingleton :: ((Int, Int) -> Bool) -> (Item, Positive Int) -> Bool
qualityCheckSingleton successCondition = (\(i, d)->
           let days = (getPositive d)
               iterations = take days $ iterate updateQuality [i]
               areSingleton = all (\x -> length x == 1) iterations
               iterationQualities = iterations >>= (fmap getQuality)
               successCondition' = all successCondition $ iterationQualities `zip` (tail iterationQualities)
           in areSingleton && successCondition')

qualityCheckAll :: (Int -> Bool) -> Int -> [Item] -> Bool
qualityCheckAll successCondition days =
  (\is ->
      let iterations = take days $ iterate updateQuality is
          iterationQualities = iterations >>= (fmap getQuality)
          successCondition' = all successCondition iterationQualities
      in successCondition')
