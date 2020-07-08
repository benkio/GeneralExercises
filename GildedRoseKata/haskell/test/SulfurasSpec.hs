module SulfurasSpec (spec) where

import GildedRose
import Generators
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "SulfurasSpec" $ do
  it "should never change in quality" $ property $ do
    forAll (itemGen "Sulfuras, Hand of Ragnaros")
      (\(i, d)->
          let days = (getPositive d)
              expectedQuality = getQuality i
              successCondition = \r -> length r == 1 && expectedQuality == getQuality (r !! 0)
              result = foldl (\acc iteration -> acc && successCondition iteration) True $ take days (iterate updateQuality [i])
          in result)
