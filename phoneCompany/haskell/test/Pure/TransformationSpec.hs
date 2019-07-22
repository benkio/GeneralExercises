module Pure.TransformationSpec where

import           Pure.Domain
import           Pure.Transformation
import           Pure.DomainSpec hiding (spec)
import           Test.Hspec
import           Test.QuickCheck
import           Data.Maybe
import           Data.Text (unpack, pack)

generateCallLog :: Bool -> Gen CallLog
generateCallLog isValid = do
  costumerId <- (listOf (elements ['a'..'z'])) :: Gen String
  duration <- generateDurationString Nothing
  nr <- if isValid then generateNumber else fmap getASCIIString (arbitrary :: Gen ASCIIString)
  return $ CallLog costumerId nr duration

spec :: Spec
spec = describe "validateInput" $ do
  it "should return a Call if the input is correct" $ property $
    forAll (generateCallLog True) (isJust . validateInput)
  it "should return Nothing if the input is incorrect" $ property $
    forAll (generateCallLog False) (isNothing . validateInput)

main :: IO ()
main = hspec spec
