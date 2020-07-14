module QualitySpec (spec) where

import Generators
import GildedRose
import Test.Hspec
import Test.QuickCheck
import PropertyChecks
import SuccessConditions

qualityNeverNegative' :: ([Item], Positive Int) -> Bool
qualityNeverNegative' (is, d) = qualityCheckAll qualityNeverNegative (getPositive d) is

qualityLessThen50' :: ([Item], Positive Int) -> Bool
qualityLessThen50' (is, d) = qualityCheckAll qualityLessThen50 (getPositive d) is

qualityDegradesTwiceAsFast :: ([Item], Positive Int) -> Bool
qualityDegradesTwiceAsFast (is, d) = qualityCheck qualityDegradesExpired False (getPositive d) is

spec :: Spec
spec = describe "QualitySpec" $ do
  it "Quality should never be negative" $ property $ do
    forAll (allItemsGen allItemGen) qualityNeverNegative'
  it "Quality should never be > 50" $ property $ do
    forAll (allItemsGen allItemGen) qualityLessThen50'
  it "Once the sell by date has passed, Quality degrades twice as fast" $ do
    forAll (allItemsGen allItemGenExpired) qualityDegradesTwiceAsFast
