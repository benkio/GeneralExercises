module AgedBrieSpec (spec) where

import GildedRose
import Generators
import Test.Hspec
import Test.QuickCheck

ascendingQualty :: (Int, Int) -> Bool
ascendingQualty (oldQ, newQ) =
  oldQ < newQ || (oldQ == 50 && newQ == 50)

spec :: Spec
spec = describe "AgedBrieSpec" $ do
  it "should always increase in quality" $ property $ do
    forAll (itemGen "Aged Brie")
      (\(i, d)->
          let days = (getPositive d)
              iterations = take days $ iterate updateQuality [i]
              areSingleton = all (\x -> length x == 1) iterations
              iterationQualities = iterations >>= (fmap getQuality)
              iterationQualityAscending = all ascendingQualty $ iterationQualities `zip` (tail iterationQualities)
          in areSingleton && iterationQualityAscending)
