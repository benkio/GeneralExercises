module SulfurasSpec (spec) where

import GildedRose
import Item
import Generators
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "SulfurasSpec" $
  it "should never change in quality" $ property $
    forAll sulfurasGen
      (\(i, d)->
          let days = getPositive d
              expectedQuality = (valueQ . getQuality) i
              successCondition r = length r == 1 && expectedQuality == (valueQ. getQuality . head) r
              result = foldl (\acc iteration -> acc && successCondition iteration) True $ take days (iterate updateQuality [i])
          in result)
