module Pure.DomainSpec where

import           Pure.Domain
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.List as L
import           Data.Maybe
import           Data.Text (unpack)
import           Text.Builder

generateNumber :: Gen String
generateNumber = do
  ns <- mapM (\_ -> choose (100, 999) :: Gen Int) [1..3]
  return $ L.foldl (++) "" ((L.intersperse "-" . fmap (show)) ns)

generateDuration :: Gen String
generateDuration = do
  hh <- (unpack . run . padFromLeft 2 '0' . string . show) <$> (choose (0, 24) :: Gen Int)
  mm <- (unpack . run . padFromLeft 2 '0' . string . show) <$> (choose (0, 60) :: Gen Int)
  ss <- (unpack . run . padFromLeft 2 '0' . string . show) <$> (choose (0, 60) :: Gen Int)
  return $ hh ++ ":" ++ mm ++ ":" ++ ss
  
spec :: Spec
spec =
  describe "Domain" $ do
    describe "number" $ do
      it "should return Just Text if the input string is a valid number" $ property $
        forAll generateNumber (isJust. number)
      it "should return Nothing if the input string is not a valid number" $ property $
        forAll (arbitrary :: Gen ASCIIString) (isNothing . number . getASCIIString)
    describe "parseDuration" $ do
      it "should return a Just duration if the input string is a valid duration" $
        forAll generateDuration (isJust. parseDuration)
      it "should return Nothing if the input string is not a valid duration" $
        forAll (arbitrary :: Gen ASCIIString) (isNothing . parseDuration . getASCIIString)

main :: IO ()
main = hspec spec