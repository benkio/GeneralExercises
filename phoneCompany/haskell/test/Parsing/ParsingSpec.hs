module Parsing.ParsingSpec where

import           Pure.Domain
import           Parsing.ParseInput
import           Pure.DomainSpec hiding (spec)
import           Test.Hspec
import           Test.QuickCheck
import           Data.List
import           Data.Int
import           Text.Printf
import           Data.Hourglass
import           Data.Text (unpack)

newtype TestHours = TestHours Hours
newtype TestMinutes = TestMinutes Minutes
newtype TestSeconds = TestSeconds Seconds

instance Show TestHours where
  show (TestHours (Hours s)) = show s

instance Show TestMinutes where
  show (TestMinutes (Minutes s)) = show s

instance Show TestSeconds where
  show (TestSeconds (Seconds s)) = show s


generateCallLog :: Bool -> Gen String
generateCallLog isValid = do
  costumerId <- ((listOf1 . elements) ['a'..'z']) :: Gen String
  called <- if (isValid) then generateNumber else ((listOf . elements) ['a'..'z']) :: Gen String
  duration <- generateDurationString Nothing
  return $ (concat . (intersperse " ")) [costumerId, called, duration]

spec :: Spec
spec = describe "parseCallsLog" $ do
  it "should return a CallLog when a valid input is provided" $ property $
    forAll (generateCallLog True)
    (\validInput -> do
       let [inputCostumerId, inputCalled, inputDuration] = words validInput
       writeFile "test.log" validInput
       result <- parseCallsLog "test.log"
       length result `shouldBe` 1
       (clCostumerId . head) result `shouldBe` inputCostumerId
       (clCalled . head) result `shouldBe` inputCalled
       (clDuration .head) result `shouldBe` inputDuration
    )
  it "should return a list of CallLog of the same size of the input list" $ property $
    forAll (listOf (generateCallLog True))
    (\listValidInput -> do
        let input = (concat . intersperse "\n") listValidInput
        writeFile "test.log" input
        result <- parseCallsLog "test.log"
        length result `shouldBe` length listValidInput
        )
  describe "parseCall" $ do
    it "should return a Call when a valid input is provided" $ property $
      forAll (generateCallLog True)
      (\validInput -> do
         let [inputCostumerId, inputCalled, inputDuration] = words validInput
         writeFile "test.log" validInput
         result <- parseCalls "test.log"
         length result `shouldBe` 1
         (costumerId . head) result `shouldBe` inputCostumerId
         (unpack. called . head) result `shouldBe` inputCalled
         ((\d -> ((printf "%02s") . show . TestHours) (durationHours d) ++ ":"
            ++ ((printf "%02s") . show . TestMinutes) (durationMinutes d) ++ ":"
            ++ ((printf "%02s") . show . TestSeconds) (durationSeconds d)) . duration . head) result `shouldBe` inputDuration
      )
  it "should return a list of CallLog of the same size of the input list" $ property $
    forAll (listOf (generateCallLog True))
    (\listValidInput -> do
        let input = (concat . intersperse "\n") listValidInput
        writeFile "test.log" input
        result <- parseCalls "test.log"
        length result `shouldBe` length listValidInput
        )
  it "should return an empty list if the input is invalid" $ property $
    forAll (generateCallLog False)
    (\invalidInput -> do
        writeFile "test.log" invalidInput
        result <- parseCalls "test.log"
        null result `shouldBe` True
       )
  it "should return an empty list if the input is invalid (List)" $ property $
    forAll (listOf (generateCallLog False))
    (\invalidInput -> do
        let input = (concat . intersperse "\n") invalidInput
        writeFile "test.log" input
        result <- parseCalls "test.log"
        null result `shouldBe` True
       )


main :: IO ()
main = hspec spec
