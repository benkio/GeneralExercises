module BackstagePassesSpec (spec) where

import GildedRose
import Generators
import Test.Hspec
import Test.QuickCheck
import SuccessConditions
import PropertyChecks

spec :: Spec
spec = describe "BackstagePassesSpec" $ do
  describe "should increase in quality" $ do
    it "by one if the expiration date is far away (> 10)" $ property $ do
      forAll (backstagePassesFarGen) $ qualityCheckSingleton ascendingQualty1
    it "by two if the expiration date is close (5-10)" $ property $ do
      forAll (backstagePassesCloseGen) $ qualityCheckSingleton ascendingQualty2
    it "by three if the expiration date is closest (0-5)" $ property $ do
      forAll (backstagePassesCloseGen) $ qualityCheckSingleton ascendingQualty3
  describe "should have quality == 0" $ do
    it "When it expires" $ do
      forAll (backstagePassesWillExpireGen)
        (\(i, d)->
          let days = (getPositive d)
              successCondition = \r -> length r == 1 && 0 == getQuality (r !! 0)
              result = foldl (\acc iteration -> acc || successCondition iteration) False $ take days (iterate updateQuality [i])
          in result)
