module PropertyChecks where

import Test.QuickCheck
import GildedRose
import Generators

qualityCheck :: ((Int, Int) -> Bool) -> Bool -> Int -> [Item] -> Bool
qualityCheck successCondition isSingleton days is =
           let iterations = take days $ iterate updateQuality is
               areSingleton = all (\x -> length x == 1) iterations
               iterationQualities = zipWith zip iterations (tail iterations) >>= fmap (\t -> (getQuality (fst t), getQuality (snd t)))
               successCondition' = all successCondition iterationQualities
           in if isSingleton then areSingleton && successCondition' else successCondition'

qualityCheckSingleton :: ((Int, Int) -> Bool) -> (Item, Positive Int) -> Bool
qualityCheckSingleton successCondition (i, d) = qualityCheck successCondition True (getPositive d) [i]
  
  
qualityCheckAll :: (Int -> Bool) -> Int -> [Item] -> Bool
qualityCheckAll successCondition days is =
      let iterations = take days $ iterate updateQuality is
          iterationQualities = iterations >>= fmap getQuality
          successCondition' = all successCondition iterationQualities
      in successCondition'
