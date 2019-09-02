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
           maxElem = maximumBy (\c1 c2 -> (duration c1) `compare` (duration c2)) cs
         in result `shouldNotContain` [maxElem]
      )

main :: IO ()
main = hspec spec
