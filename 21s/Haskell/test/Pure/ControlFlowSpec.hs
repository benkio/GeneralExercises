module Pure.ControlFlowSpec where

import Data.List
import Data.Ord
import DataArbitraryInstances
import Pure.ControlFlow
import Pure.Domain
import Pure.Rules
import Test.Hspec
import Test.QuickCheck

injectPlayerHand :: [Card] -> Gen Player
injectPlayerHand playerHand = do
    playerName <- listOf (arbitrary :: Gen Char)
    return $ Player{hand = playerHand, name = playerName}

twoHandsOrdered :: Ordering -> Gen ([Card], [Card])
twoHandsOrdered ord = do
    handV <- listOf (arbitrary :: Gen Card)
    addCard <- arbitrary :: Gen Card
    return $ case ord of
        GT -> (addCard : handV, handV)
        LT -> (handV, addCard : handV)
        EQ -> (handV, handV)

generateGameState :: Ordering -> Gen GameState
generateGameState ord = do
    hands <- twoHandsOrdered ord
    playerV <- (injectPlayerHand . fst) hands
    dealerV <- (injectPlayerHand . snd) hands
    return $ GameState{properPlayer = playerV, dealerPlayer = dealerV, gameStateDeck = []}

spec :: Spec
spec =
    describe "ControlFlowSpec" $ do
        describe "Message Functions" $ do
            context "When Player has Blackjack" $ do
                it "blackjackMessage return a message containing specific keywords" $
                    property $
                        forAll
                            (arbitrary :: Gen Player)
                            ( \p ->
                                let result = (blackjackMessage True p)
                                 in isInfixOf "BLACKJACK" result && isInfixOf "Hand of " result
                            )
                it "lostMessage return a message containing specific keywords" $
                    property $
                        forAll
                            (arbitrary :: Gen Player)
                            ( \p ->
                                let result = (lostMessage True p)
                                 in isInfixOf "HAS LOST" result && isInfixOf "Hand of " result
                            )
            context "When Player hasn't Blackjack" $ do
                it "blackjackMessage return a message containing specific keywords" $
                    property $
                        forAll
                            (arbitrary :: Gen Player)
                            ( \p ->
                                let result = (blackjackMessage False p)
                                 in isInfixOf "Hand of " result
                            )
                it "lostMessage return a message containing specific keywords" $
                    property $
                        forAll
                            (arbitrary :: Gen Player)
                            ( \p ->
                                let result = (lostMessage False p)
                                 in isInfixOf "Hand of " result
                            )
        describe "Pick a Winner" $ do
            context "Return a message with specific keywords when" $ do
                it "the dealer wins over the player" $
                    property $
                        forAll (generateGameState LT) (\gs -> isInfixOf "The dealer win the game" (pickAWinner gs))
                it "the player wins over the dealer" $
                    property $
                        forAll (generateGameState GT) (\gs -> isInfixOf ((name . properPlayer) gs ++ " wins the game") (pickAWinner gs))
                it "the dealer wins over the player" $
                    property $
                        forAll (generateGameState EQ) (\gs -> isInfixOf "Tie game at " (pickAWinner gs))

main :: IO ()
main = hspec spec
