module ConjuredSpec (spec) where

import Generators
import Test.Hspec
import Test.QuickCheck
import PropertyChecks
import SuccessConditions

spec :: Spec
spec = describe "ConjuredSpec" $
  it "should degrade in quality twice as fast" $ property $
    forAll conjuredGen (\(i, d) -> qualityCheck qualityDegradesExpired True (getPositive d) [i])
