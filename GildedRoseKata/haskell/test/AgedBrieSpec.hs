module AgedBrieSpec (spec) where

import Generators
import Test.Hspec
import Test.QuickCheck
import SuccessConditions
import PropertyChecks

spec :: Spec
spec = describe "AgedBrieSpec" $ do
  it "should always increase in quality" $ property $ do
    forAll (itemGen "Aged Brie") $ qualityCheck ascendingQualty1
