{-# LANGUAGE FlexibleContexts #-}

module IO.TwentyOneSpec where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Random.Lazy
import Control.Monad.State.Lazy
import Data.Either
import Data.Functor.Identity
import Data.List
import IO.Algebras
import IO.EffectfulInstances
import IO.TwentyOne
import Pure.Domain
import Test.Hspec
import Test.QuickCheck

dealerWinnerGameState :: GameState
dealerWinnerGameState =
    (gameState [])
        { dealerPlayer = dealer{hand = [Card{cValue = Seven, cType = Diamonds}]}
        }

playerWinnerGameState :: GameState
playerWinnerGameState =
    (gameState [])
        { properPlayer = sam{hand = [Card{cValue = Seven, cType = Diamonds}]}
        }
tieGameState :: GameState
tieGameState = (gameState [])

callWinnerPhase :: GameState -> (GameState, [String])
callWinnerPhase gs = runState (callWinnerPhase' gs) []

callWinnerPhase' :: GameState -> State [String] GameState
callWinnerPhase' gs = do
    put []
    result <- execStateT winnerPhase gs
    return result

callplayerDrawingState :: Player -> GameState -> Either IOException (GameState, [String])
callplayerDrawingState p gs = evalRand (runExceptT $ runStateT (execStateT (playerDrawingPhase p) gs) []) ()

callplayerDrawingStateStack :: Player -> GameState -> Identity (GameState, [String])
callplayerDrawingStateStack p gs = runStateT (execStateT (playerDrawingPhase p) gs) []

callNewCardToPlayer ::
    (GameState -> Player) ->
    (GameState -> Player -> Deck -> GameState) ->
    GameState ->
    GameState
callNewCardToPlayer playerExtraction gameStateUpdate initialGamestate =
    evalRand (execStateT (newCardToPlayer playerExtraction gameStateUpdate) initialGamestate) ()
callSetup :: GameState -> Either IOException (GameState, [String])
callSetup gs = runStateT (execStateT setup gs) []

callSetupStack :: GameState -> Identity (GameState, [String])
callSetupStack gs = runStateT (execStateT setup gs) []

callDrawACard :: GameState -> (Card, GameState)
callDrawACard gs = evalRand (runStateT (drawACard) gs) ()

callInitialState :: GameState -> GameState
callInitialState gs = evalRand (execStateT (initialState) gs) ()

spec :: Spec
spec =
    describe "TwentyOneSpec" $ do
        winnerPhaseSpec
        drawTurnPatternSpec
        playerDrawingPhaseSpec
        newCardToPlayerSpec
        setupSpec
        drawACardSpec
        initialStateSpec

winnerPhaseSpec :: Spec
winnerPhaseSpec =
    describe "winnerPhase" $ do
        it "prints out a dealer winning message when the dealer wins" $ do
            let stack = snd $ callWinnerPhase dealerWinnerGameState
                result = null stack == False && isInfixOf "The dealer win the game: " (head stack)
            result `shouldBe` True
        it "prints out a player winning message when the player wins" $ do
            let stack = snd $ callWinnerPhase playerWinnerGameState
                result = null stack == False && isInfixOf "wins the game: " (head stack)
            result `shouldBe` True
        it "prints out a tie message" $ do
            let stack = snd $ callWinnerPhase tieGameState
                result = null stack == False && isInfixOf "Tie game at " (head stack)
            result `shouldBe` True

drawTurnPatternSpec :: Spec
drawTurnPatternSpec =
    describe "drawTurnPattern" $ do
        it "Return the expected player if the exit condition is true" $ do
            let initialGs = gameState []
                result =
                    runRand
                        ( runStateT (drawTurnPattern (\_ -> True) properPlayer (\_ _ _ -> initialGs)) initialGs
                        )
                        ()
            result `shouldBe` ((sam, initialGs), ())
        it "Return: the expected player with the first card of the deck and an updated Gamestate if the exit condition is not met once" $ do
            let initialGs = gameState deck
                exitCond = (== 51) . length . gameStateDeck
                updateStateFn = \gs p d -> gs{properPlayer = p, gameStateDeck = d}
                result =
                    runRand
                        ( runStateT (drawTurnPattern exitCond properPlayer updateStateFn) initialGs
                        )
                        ()
                expectedPlayer = sam{hand = [Card{cValue = Two, cType = Clubs}]}
                expectedGs = (gameState (tail deck)){properPlayer = expectedPlayer}
            result `shouldBe` ((expectedPlayer, expectedGs), ())

playerDrawingPhaseSpec :: Spec
playerDrawingPhaseSpec =
    describe "playerDrawingPhase" $ do
        let initialGs = gameState deck
        context "When the player in input has lost" $ do
            let lostPlayer = sam{hand = take 10 deck}
            it "Throw an error and add a println" $ do
                let result = callplayerDrawingState lostPlayer initialGs
                let resultStack = (snd . runIdentity) $ callplayerDrawingStateStack lostPlayer initialGs
                (isLeft result) `shouldBe` True
                ((isInfixOf "Player Lost" . show . fromLeft (error "test failed")) result) `shouldBe` True
                ((isInfixOf "HAS LOST!!!" . head) resultStack) `shouldBe` True
        context "When the player in input is still in game" $
            it "Add a println and return ()" $ do
                let result = callplayerDrawingState sam initialGs
                let resultStack = (snd . runIdentity) $ callplayerDrawingStateStack sam initialGs
                (isRight result) `shouldBe` True
                ((isInfixOf "Hand of Sam: " . head) resultStack) `shouldBe` True

newCardToPlayerSpec :: Spec
newCardToPlayerSpec =
    describe "newCardToPlayer" $ do
        let initialGs = gameState deck
            updateGSSam = (properPlayer, (\gst p d' -> gst{properPlayer = p, gameStateDeck = d'}))
            updateGSDealer = (dealerPlayer, (\gst p d' -> gst{dealerPlayer = p, gameStateDeck = d'}))
        it "should add a new card to the player hands when called with player functions" $ do
            let newGamestate = callNewCardToPlayer (fst updateGSSam) (snd updateGSSam) initialGs
                expectedPlayer = sam{hand = [(head deck)]}
            properPlayer newGamestate `shouldBe` expectedPlayer
        it "should add a new card to the dealer hands when called with player functions" $ do
            let newGamestate = callNewCardToPlayer (fst updateGSDealer) (snd updateGSDealer) initialGs
                expectedPlayer = dealer{hand = [(head deck)]}
            dealerPlayer newGamestate `shouldBe` expectedPlayer

setupSpec :: Spec
setupSpec =
    describe "setup" $ do
        context "in the happy path" $ do
            let initialGoodGs = gameState deck
            it "should always print the hands of both players to stdout" $ do
                let stack = (snd . runIdentity . callSetupStack) initialGoodGs
                length stack `shouldBe` 2
                (foldl (&&) True . map (isInfixOf "Hand of")) stack `shouldBe` True
            it "should return the same gamestate in input" $ do
                let result = callSetup initialGoodGs
                isRight result `shouldBe` True
                (fst . fromRight undefined) result `shouldBe` initialGoodGs
        context "if someone has a blackjack" $ do
            let blackJackPlayer = sam{hand = [Card{cValue = Ace, cType = Clubs}, Card{cValue = Queen, cType = Hearts}]}
                initialFailGs = (gameState deck){properPlayer = blackJackPlayer}
            it "should always print the blackjack expected message to stdout" $ do
                let stack = (snd . runIdentity . callSetupStack) initialFailGs
                length stack `shouldBe` 2
                (foldl (||) True . map (isInfixOf "BLACKJACK!!")) stack `shouldBe` True
            it "should return the error" $ do
                let result = callSetup initialFailGs
                isLeft result `shouldBe` True
                fromLeft undefined result `shouldBe` (userError . show) Blackjack

drawACardSpec :: Spec
drawACardSpec =
    describe "drawACard" $ do
        context "current GameState has a non-empty deck" $ do
            let initialGs = gameState deck
            it "should return the top card from the GameState Deck and set a new GameState" $ do
                let (card, gs) = callDrawACard initialGs
                card `shouldBe` (head deck)
                gs `shouldBe` initialGs{gameStateDeck = (tail deck)}
        context "current GameState has an empty deck" $ do
            let emptyDeckGs = gameState []
                shuffledDeck = [Card{cValue = Four, cType = Diamonds}, Card{cValue = Nine, cType = Clubs}, Card{cValue = Four, cType = Hearts}, Card{cValue = Two, cType = Hearts}, Card{cValue = Jack, cType = Diamonds}, Card{cValue = King, cType = Hearts}, Card{cValue = Nine, cType = Spades}, Card{cValue = Five, cType = Clubs}, Card{cValue = Five, cType = Diamonds}, Card{cValue = Two, cType = Clubs}, Card{cValue = Five, cType = Spades}, Card{cValue = Six, cType = Clubs}, Card{cValue = Six, cType = Diamonds}, Card{cValue = Six, cType = Hearts}, Card{cValue = Nine, cType = Hearts}, Card{cValue = Four, cType = Spades}, Card{cValue = Seven, cType = Clubs}, Card{cValue = Seven, cType = Diamonds}, Card{cValue = Queen, cType = Diamonds}, Card{cValue = Seven, cType = Hearts}, Card{cValue = Two, cType = Spades}, Card{cValue = Eight, cType = Clubs}, Card{cValue = Eight, cType = Diamonds}, Card{cValue = Ten, cType = Diamonds}, Card{cValue = Eight, cType = Hearts}, Card{cValue = Two, cType = Diamonds}, Card{cValue = Nine, cType = Diamonds}, Card{cValue = Ten, cType = Clubs}, Card{cValue = Ten, cType = Hearts}, Card{cValue = Four, cType = Clubs}, Card{cValue = Jack, cType = Clubs}, Card{cValue = Jack, cType = Hearts}, Card{cValue = Jack, cType = Spades}, Card{cValue = Three, cType = Clubs}, Card{cValue = King, cType = Diamonds}, Card{cValue = Queen, cType = Clubs}, Card{cValue = Three, cType = Hearts}, Card{cValue = Queen, cType = Hearts}, Card{cValue = Queen, cType = Spades}, Card{cValue = Ace, cType = Clubs}, Card{cValue = Ace, cType = Diamonds}, Card{cValue = Ace, cType = Hearts}, Card{cValue = Ace, cType = Spades}, Card{cValue = Three, cType = Diamonds}, Card{cValue = Five, cType = Hearts}, Card{cValue = Seven, cType = Spades}, Card{cValue = Ten, cType = Spades}, Card{cValue = King, cType = Spades}, Card{cValue = Six, cType = Spades}, Card{cValue = Three, cType = Spades}, Card{cValue = King, cType = Clubs}, Card{cValue = Eight, cType = Spades}]
            it "should return a card with a GameState deck shuffled" $ do
                let (card, gs) = callDrawACard emptyDeckGs
                card `shouldBe` (head shuffledDeck)
                gs `shouldBe` emptyDeckGs{gameStateDeck = (tail shuffledDeck)}

initialStateSpec :: Spec
initialStateSpec =
    describe "initialStateSpec" $ do
        let initialGs = gameState deck
        it "should return a GameState containing the expected deck and players with expected hands" $ do
            let result = callInitialState initialGs
            (properPlayer result) `shouldBe` sam{hand = take 2 deck}
            (dealerPlayer result) `shouldBe` dealer{hand = (take 2 . drop 2) deck}
            (gameStateDeck result) `shouldBe` (drop 4 deck)

main :: IO ()
main = hspec spec
