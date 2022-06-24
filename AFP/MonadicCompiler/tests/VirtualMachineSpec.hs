module VirtualMachineSpec (spec) where

import SourceLanguage
import Test.Hspec
import TestData (factorialCode, factorialFinalMem, factorialProg)
import VirtualMachine

spec :: Spec
spec = do
  describe "comp" $ do
    it "produce the expected Factorial code starting from the input program" $ do
      fmap (comp . factorialProg) [1 .. 10] `shouldBe` fmap factorialCode [1 .. 10]
    it "produce the expected Assign code" $ do
      comp (Seqn [Assign 'A' (Val 2), Assign 'B' (Var 'C')]) `shouldBe` [PUSH 2, POP 'A', PUSHV 'C', POP 'B']
    it "produce the expected If code" $ do
      comp (If (Val 0) (Assign 'A' (Val 2)) (Assign 'B' (Var 'C'))) `shouldBe` [PUSH 0, JUMPZ 0, PUSH 2, POP 'A', JUMP 1, LABEL 0, PUSHV 'C', POP 'B', LABEL 1]
    it "produce the expected While code" $ do
      comp (While (Var 'A') (Assign 'A' (App Sub (Var 'A') (Val 1)))) `shouldBe` [LABEL 0, PUSHV 'A', JUMPZ 1, PUSHV 'A', PUSH 1, DO Sub, POP 'A', JUMP 0, LABEL 1]
  describe "exec" $ do
    it "produce the expected Factorial Mem from the input code" $ do
      fmap (exec . factorialCode) [1 .. 10] `shouldBe` fmap factorialFinalMem [1 .. 10]
    it "produce the expected Assign Mem" $ do
      exec [PUSH 2, POP 'A'] `shouldBe` [('A', 2)]
    it "produce the expected If Mem" $ do
      exec [PUSH 1, JUMPZ 0, PUSH 2, POP 'A', JUMP 1, LABEL 0, PUSHV 'C', POP 'B', LABEL 1] `shouldBe` [('A', 2)]
    it "produce the expected While Mem" $ do
      exec [PUSH 2, POP 'A', PUSH 0, POP 'B', LABEL 0, PUSHV 'A', JUMPZ 1, PUSHV 'A', PUSH 1, DO Sub, POP 'A', PUSHV 'B', PUSH 1, DO Add, POP 'B', JUMP 0, LABEL 1] `shouldBe` [('B', 2), ('A', 0)]
