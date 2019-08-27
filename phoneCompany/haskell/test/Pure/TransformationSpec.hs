module Pure.TransformationSpec where

import           Pure.Domain
import           Pure.Transformation
import           Pure.DomainSpec hiding (spec)
import           Test.Hspec
import           Data.Text (Text, pack)
import           Test.QuickCheck
import           Data.Maybe
import           Data.Text (unpack, pack)
import           Data.Map.Strict
import           Data.Hourglass
import           Data.List (nub)

generateCallLog :: Bool -> Gen CallLog
generateCallLog isValid = do
  costumerId <- (listOf (elements ['a'..'z'])) :: Gen String
  duration <- generateDurationString Nothing
  nr <- if isValid then generateNumber else fmap getASCIIString (arbitrary :: Gen ASCIIString)
  return $ CallLog costumerId nr duration

generateCalls :: (String -> Text -> Duration -> Call) -> Gen Call
generateCalls constructor = do
  costumerId <- (listOf (elements ['a'..'z'])) :: Gen String
  duration <- generateDuration Nothing
  numberGen <- fmap (\n -> pack n) generateNumber
  return $ constructor costumerId numberGen duration

calls :: Gen [Call]
calls = (listOf $ oneof [generateCalls OverflowCall, generateCalls StandardRateCall])

spec :: Spec
spec = describe "validateInput" $ do
  it "should return a Call if the input is correct" $ property $
    forAll (generateCallLog True) (isJust . validateInput)
  it "should return Nothing if the input is incorrect" $ property $
    forAll (generateCallLog False) (isNothing . validateInput)
  describe "groupByCostumer" $ do
    it "should contain all the input calls as elements" $ property $
      forAll calls
      (\input -> (length . concat . elems . groupByCostumer) input == length input)
    it "should contain all the input costumers as keys" $ property $
      forAll calls
      (\input -> (length . keys . groupByCostumer) input == (length . nub . fmap (costumerId)) input)

main :: IO ()
main = hspec spec
