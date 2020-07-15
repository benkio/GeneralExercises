module AgedBrieSpec (spec) where

import Generators
import Test.Hspec
import Test.QuickCheck
import SuccessConditions
import PropertyChecks

spec :: Spec
spec = describe "AgedBrieSpec" $
  it "should always increase in quality" $ property $
    forAll agedBrieGen $ qualityCheckSingleton ascendingQualty1
