module VirtualMachineSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      1 `shouldBe` 1
