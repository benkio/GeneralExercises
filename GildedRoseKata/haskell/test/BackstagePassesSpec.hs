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
    it "by two if the expiration date is close (5-10)" $ property $ do
      forAll (backstagePassesCloseGen) $ qualityCheck ascendingQualty2
    it "by three if the expiration date is closest (0-5)" $ property $ do
      forAll (backstagePassesCloseGen) $ qualityCheck ascendingQualty3
  describe "should have quality == 0" $ do
    it "When it expires" $ do
      pending
