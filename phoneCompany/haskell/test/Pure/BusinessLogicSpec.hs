{-# LANGUAGE DataKinds #-}

module Pure.BusinessLogicSpec where

import Data.Hourglass
import Data.List
import Data.Map.Strict (
    Map,
    elems,
    empty,
    fromList,
    singleton,
 )
import Data.Maybe
import Money
import Pure.BusinessLogic
import Pure.Domain
import Pure.TransformationSpec (calls)
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (filter)

standardCallTest1 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:02:00")
standardCallTest2 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:01:00")
standardCallTest3 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:00:30")

overflowCallTest1 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:05:00")
overflowCallTest2 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:04:00")
overflowCallTest3 = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:03:30")

callTest1 =
    [ standardCallTest1
    , standardCallTest2
    , standardCallTest3
    , overflowCallTest1
    , overflowCallTest2
    , overflowCallTest3
    ]

callTest2 =
    [ ("costumer1", [standardCallTest1, overflowCallTest1, overflowCallTest2]) -- 1680
    , ("costumer2", [standardCallTest2, overflowCallTest2, overflowCallTest1]) -- 1380
    , ("costumer3", [standardCallTest3, overflowCallTest3, overflowCallTest1]) -- 1140
    ]

spec :: Spec
spec =
    describe "filterLongerCall" $ do
        it "should return an empty list if we serve it as input" $ do
            filterLongerCall empty `shouldBe` empty
        it "should remove the maximum element from the call list" $
            property $
                forAll
                    (calls `suchThat` ((not . null)))
                    ( \cs ->
                        let
                            result :: [Call]
                            result = (head . elems . filterLongerCall) (singleton "testKey" cs)
                            maxElem = maximumBy (\c1 c2 -> (invoice c1) `compare` (invoice c2)) cs
                         in
                            result `shouldNotContain` [maxElem]
                    )
        describe "invoice" $ do
            it "should convert a Standard Rate Call to Money based on the standard rate" $ do
                invoice standardCallTest1 `shouldBe` (Money.discrete 600 :: Money.Discrete "GBP" "penny")
            it "should convert a Overflow Rate Call to Money based on the standard rate" $ do
                invoice overflowCallTest1 `shouldBe` (Money.discrete 1260 :: Money.Discrete "GBP" "penny")
        describe "calculateCostumerInvoice" $ do
            it "should calculate the right amount of invoice given a list of calls" $ do
                calculateCostumerInvoice (singleton "testCostumer" callTest1) `shouldBe` (singleton "testCostumer" (Money.discrete 4380))
            it "should return 0 if the input is empty" $ do
                calculateCostumerInvoice (singleton "testCostumer" []) `shouldBe` (singleton "testCostumer" (Money.discrete 0))
        describe "businessLogic" $ do
            it "should return 0 if the input is empty" $
                businessLogic (singleton "testCostumer" []) `shouldBe` (singleton "testCostumer" (Money.discrete 0))
            it "should return 0 if the input list contains only one element" $
                businessLogic (singleton "testCostumer" [standardCallTest1]) `shouldBe` (singleton "testCostumer" (Money.discrete 0))
            it "should return the expected invoice given a specific input 1" $
                businessLogic (singleton "testCostumer" callTest1) `shouldBe` (singleton "testCostumer" (Money.discrete 3120))
            it "should return the expected invoice given a specific input 2" $
                businessLogic (fromList callTest2)
                    `shouldBe` fromList
                        [ ("costumer1", Money.discrete 1680)
                        , ("costumer2", Money.discrete 1380)
                        , ("costumer3", Money.discrete 1140)
                        ]

main :: IO ()
main = hspec spec
