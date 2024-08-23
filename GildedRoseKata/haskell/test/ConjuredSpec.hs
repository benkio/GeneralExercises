module ConjuredSpec (spec) where

import Generators
import PropertyChecks
import SuccessConditions
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "ConjuredSpec" $
        it "should degrade in quality twice as fast" $
            property $
                forAll conjuredGen (\(i, d) -> qualityCheck qualityDegradesExpired True (getPositive d) [i])
