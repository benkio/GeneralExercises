module PropertyChecks where

import Test.QuickCheck
import GildedRose
import Generators

qualityCheck :: ((Int, Int) -> Bool) -> (Item, Positive Int) -> Bool
qualityCheck successCondition = (\(i, d)->
           let days = (getPositive d)
               iterations = take days $ iterate updateQuality [i]
               areSingleton = all (\x -> length x == 1) iterations
               iterationQualities = iterations >>= (fmap getQuality)
               iterationQualityAscending = all successCondition $ iterationQualities `zip` (tail iterationQualities)
           in areSingleton && iterationQualityAscending)
