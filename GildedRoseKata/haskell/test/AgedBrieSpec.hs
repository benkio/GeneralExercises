module AgedBrieSpec (spec) where

import Generators
import PropertyChecks
import SuccessConditions
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "AgedBrieSpec" $
        it "should always increase in quality" $
            property $
                forAll agedBrieGen $
                    qualityCheckSingleton ascendingQualty1
