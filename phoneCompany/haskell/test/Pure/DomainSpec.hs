module Pure.DomainSpec where

import           Pure.Domain
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.List as L
import           Data.Maybe
import           Data.Text (unpack, pack)
import           Text.Builder
import           Data.Hourglass
import           Data.Int

generateNumber :: Gen String
generateNumber = do
  ns <- mapM (\_ -> choose (100, 999) :: Gen Int) [1..3]
  return $ L.foldl (++) "" ((L.intersperse "-" . fmap (show)) ns)

generateDurationString :: Maybe (Int, Int, Int) -> Gen String
generateDurationString maybeLimits = do
  duration <- generateDuration maybeLimits
  return $ (convertToString . fromEnum . durationHours) duration ++ ":"
    ++ (convertToString . fromEnum . durationMinutes) duration ++ ":"
    ++ (convertToString . fromEnum . durationSeconds) duration
  where
    convertToString :: Int -> String
    convertToString = (unpack . run . padFromLeft 2 '0' . string . show)

generateDuration :: Maybe (Int, Int, Int) -> Gen Duration
generateDuration maybeLimits = do
  let (seconds, minutes, hours) = maybe (60, 60, 24) id maybeLimits
  ss <- toEnum <$> (choose (0, seconds) :: Gen Int)
  mm <- toEnum <$> (choose (0, minutes) :: Gen Int)
  hh <- toEnum <$> (choose (0, hours)   :: Gen Int)
  return $ Duration {
    durationHours=Hours hh,
    durationMinutes=Minutes mm,
    durationSeconds=Seconds ss,
    durationNs=NanoSeconds 0
    }

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
        forAll (generateDurationString Nothing) (isJust. parseDuration)
      it "should return Nothing if the input string is not a valid duration" $
        forAll (arbitrary :: Gen ASCIIString) (isNothing . parseDuration . getASCIIString)
    describe "isWithinStandardRate" $ do
      it "should return true if the input duration is within 3 minutes" $ property $ do
        forAll (generateDuration (Just (60, 2, 0))) ((== True) . isWithinStandardRate)
      it "should return false if the input duration exceed 3 minutes" $ property $ do
        forAll (generateDuration Nothing) ((== False) . isWithinStandardRate . (<> mempty { durationMinutes = 3 }))
    describe "call" $ do
      it "should return a StandardCall if the input duration is less then 3 minutes" $ property $ do
        forAll ( (,) . pack <$> generateNumber <*> generateDuration (Just (60, 2, 0)))
          (\t -> isStandardCall (call "test" (fst t) (snd t)) == True)
      it "should return an OverflowCall if the input dration is longer then 3 minute" $ property $ do
        forAll ( (,) . pack <$> generateNumber <*> generateDuration Nothing)
          (\t -> isStandardCall (call "test" (fst t) (((<> mempty { durationMinutes = 3 }). snd) t)) == False)

isStandardCall :: Call -> Bool
isStandardCall (StandardRateCall _ _ _) = True
isStandardCall _ = False

main :: IO ()
main = hspec spec
