module BackstagePassesSpec (spec) where

import Generators
import Test.Hspec
import Test.QuickCheck
import SuccessConditions
import PropertyChecks

spec :: Spec
spec = describe "BackstagePassesSpec" $ do
  describe "should increase in quality" $ do
    it "by one if the expiration date is far away (> 10)" $ property $ do
      forAll (backstagePassesFarGen) $ qualityCheck ascendingQualty1
