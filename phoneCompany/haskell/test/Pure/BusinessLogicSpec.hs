module Pure.BusinessLogicSpec where

import Prelude hiding (filter)
import Pure.Domain
import Pure.BusinessLogic
import Pure.TransformationSpec (calls)
import Data.Map.Strict (
  Map,
  empty,
  elems,
  singleton
  )
import Data.List
import           Data.Hourglass
-- import Money
import           Test.Hspec
import           Test.QuickCheck
import Data.Maybe

spec :: Spec
spec =
  describe "filterLongerCall" $ do
    it "should return an empty list if we serve it as input" $ do
      filterLongerCall empty `shouldBe` empty
    it "should remove the maximum element from the call list" $ property $
      forAll (calls `suchThat` ((not. null)))
      (\cs ->
         let
           result :: [Call]
           result = (head . elems .filterLongerCall) (singleton "testKey" cs)
           maxElem = maximumBy (\c1 c2 -> (invoice c1) `compare` (invoice c2)) cs
         in result `shouldNotContain` [maxElem]
      )
    describe "invoice" $ do
      let stardardCallTest = call "costumer" ((fromJust . number) "222-222-222") ((fromJust . parseDuration) "00:02:00")
      let overflowCallTest = call "costumer" ((fromJust . number) "222-222-222") ((fromJust. parseDuration) "00:05:00")
      it "should convert a Standard Rate Call to Money based on the standard rate" $ pending
      it "should convert a Overflow Rate Call to Money based on the standard rate" $ pending


main :: IO ()
main = hspec spec
