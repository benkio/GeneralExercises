module Pure.RulesSpec where

import Data.List
import DataArbitraryInstances
import Pure.Domain
import Pure.Rules
import Test.Hspec
import Test.QuickCheck

generateCards :: [CardValue] -> Int -> Gen [Card]
generateCards excludeCards numberOfCards =
    suchThat ((vector numberOfCards) :: Gen [Card]) (\cs -> null $ excludeCards `intersect` map cValue cs)

validPairOfCards :: Gen [Card]
validPairOfCards = generateCards [Ace] 2

validPairOfCardsLowScore :: Gen [Card]
validPairOfCardsLowScore = generateCards (enumFromTo Nine Ace) 2

invalidCardHand :: Gen [Card]
invalidCardHand = generateCards (enumFromTo Two Five) 4

blackjackHand :: Gen [Card]
blackjackHand =
    suchThat (listOf (arbitrary :: Gen Card)) (\cs -> getHandValue cs == 21)

playerRulesCheck :: (Player -> Bool) -> [Card] -> Bool
playerRulesCheck f cs = f $ Player{name = "test", hand = cs}

spec :: Spec
spec =
    describe "Rules" $ do
        context "with a pair of cards of value under blackjack" $ do
            let cardGenerator = (oneof [validPairOfCards, validPairOfCardsLowScore])
            describe "getHandValue" $ do
                it "should return a value lower then 21" $
                    property $
                        forAll cardGenerator (\cs -> (getHandValue cs) < 21)
            describe "hasLost" $ do
                it "should return false" $
                    property $
                        forAll cardGenerator (playerRulesCheck (not . hasLost))
            describe "hasBlackjack" $ do
                it "should return false" $
                    property $
                        forAll cardGenerator (playerRulesCheck (not . hasBlackjack))
            describe "score" $ do
                it "should be equal to getHandValue unWrapped" $ do
                    forAll
                        cardGenerator
                        (\cs -> let p = Player{name = "test", hand = cs} in getHandValue cs == score p)
            describe "hasMoreThen17" $ do
                it "should return false" $
                    property $
                        forAll validPairOfCardsLowScore (playerRulesCheck (not . hasMoreThen17))
        context "Wiith a card hand exeeding 21" $ do
            describe "getHandValue" $ do
                it "should return a value bigger then 21" $
                    property $
                        forAll invalidCardHand (\cs -> (getHandValue cs) > 21)
            describe "hasLost" $ do
                it "should return false" $
                    property $
                        forAll invalidCardHand (playerRulesCheck hasLost)
            describe "hasBlackjack" $ do
                it "should return false" $
                    property $
                        forAll invalidCardHand (playerRulesCheck (not . hasBlackjack))
        context "Blackjack hand" $ do
            describe "getHandValue" $ do
                it "should return a value bigger then 21" $
                    property $
                        forAll blackjackHand (\cs -> (getHandValue cs) == 21)
            describe "hasBlackjack" $ do
                it "should return true" $
                    property $
                        forAll blackjackHand (playerRulesCheck hasBlackjack)

main :: IO ()
main = hspec spec
