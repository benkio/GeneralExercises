module VirtualMachineSpec (spec) where

import Test.Hspec
import TestData (factorialCode, factorialFinalMem, factorialProg)
import VirtualMachine (comp, exec)

spec :: Spec
spec = do
  describe "comp" $ do
    it "produce the expected code starting from the input program" $ do
      fmap (comp . factorialProg) [1 .. 10] `shouldBe` fmap factorialCode [1 .. 10]
  describe "exec" $ do
    it "produce the expected Mem from the input code" $ do
      fmap (exec . factorialCode) [1 .. 10] `shouldBe` fmap factorialFinalMem [1 .. 10]
