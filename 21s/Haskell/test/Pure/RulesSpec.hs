module Pure.RulesSpec where

import Pure.Rules
import Pure.Domain
import Data.List
import Test.Hspec
import Test.QuickCheck

instance Arbitrary CardValue where
  arbitrary = elements (enumFrom (toEnum 0))
instance Arbitrary CardType where
  arbitrary = elements (enumFrom (toEnum 0))
instance Arbitrary Card where
  arbitrary = do
    cv <- arbitrary :: Gen CardValue
    ct <- arbitrary :: Gen CardType
    return $ Card {cValue = cv, cType = ct}

pairOfCardsExcept :: [CardValue] -> Gen [Card]
pairOfCardsExcept excludeCards =
  suchThat ((vector 2) :: Gen [Card]) (\cs -> null $ excludeCards `intersect` map cValue cs)

validPairOfCards :: Gen[Card]
validPairOfCards = pairOfCardsExcept [Ace]

validPairOfCardsLowScore :: Gen [Card]
validPairOfCardsLowScore = pairOfCardsExcept (enumFromTo Nine Ace)

playerRulesCheck :: (Player -> Bool) -> [Card] -> Bool
playerRulesCheck f cs = f $Player {name="test", hand=cs}

spec :: Spec
spec =
  describe "Rules" $ do
    context "with a pair of cards of value under blackjack" $ do
      let cardGenerator = (oneof [validPairOfCards, validPairOfCardsLowScore])
      describe "getHandValue" $ do
        it "should return a value less then 21" $ property $
          forAll cardGenerator (\cs -> (getHandValue cs) < 21)
      describe "hasLost" $ do
        it "should return false" $ property $
          forAll cardGenerator (playerRulesCheck (not . hasLost))
      describe "hasBlackjack" $ do
        it "should return false" $ property $
          forAll cardGenerator (playerRulesCheck (not . hasBlackjack))
      describe "score" $ do
        it "should be equal to getHandValue unWrapped" $ do
          forAll
            cardGenerator
            (\cs -> let p = Player {name="test", hand=cs } in getHandValue cs == score p )
      describe "hasMoreThen17" $ do
        it "should return false" $ property $
          forAll validPairOfCardsLowScore (playerRulesCheck (not . hasMoreThen17))


main :: IO ()
main = hspec spec
