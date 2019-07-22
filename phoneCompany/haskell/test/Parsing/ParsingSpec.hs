module Parsing.ParsingSpec where

import           Pure.Domain
import           Parsing.ParseInput
import           Pure.DomainSpec hiding (spec)
import           Test.Hspec
import           Test.QuickCheck
import           Data.List

generateValidCallLog :: Gen String
generateValidCallLog = do
  costumerId <- ((listOf1 . elements) ['a'..'z']) :: Gen String
  called <- generateNumber
  duration <- generateDurationString Nothing
  return $ (concat . (intersperse " ")) [costumerId, called, duration]

spec :: Spec
spec = describe "parseCallsLog" $ do
  it "should return a CallLog when a valid input is provided" $ property $
    forAll (generateValidCallLog)
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
    forAll (listOf generateValidCallLog)
    (\listValidInput -> do
        let input = (concat . intersperse "\n") listValidInput
        writeFile "test.log" input
        result <- parseCallsLog "test.log"
        length result `shouldBe` length listValidInput
        )

main :: IO ()
main = hspec spec
