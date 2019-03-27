{-# LANGUAGE FlexibleContexts #-}
module IO.TwentyOneSpec where

import Pure.Domain
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State.Lazy
import Control.Monad.Random.Lazy
import Control.Monad.Except
import IO.EffectfulInstances
import IO.TwentyOne
import IO.Algebras
import Data.List
import Data.Either
import Data.Functor.Identity
import Control.Exception

dealerWinnerGameState :: GameState
dealerWinnerGameState = (gameState []) {
  dealerPlayer=dealer {hand=[Card {cValue=Seven, cType=Diamonds}]}
  }

playerWinnerGameState :: GameState
playerWinnerGameState = (gameState []) {
  properPlayer=sam {hand=[Card {cValue=Seven, cType=Diamonds}]}
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

callNewCardToPlayer :: (GameState -> Player) ->
                       (GameState -> Player -> Deck -> GameState) ->
                       GameState ->
                       GameState
callNewCardToPlayer playerExtraction gameStateUpdate initialGamestate =
  evalRand (execStateT (newCardToPlayer playerExtraction gameStateUpdate) initialGamestate) ()
callSetup :: GameState -> Either IOException (GameState, [String])
callSetup gs = runStateT (execStateT setup gs) []

callSetupStack :: GameState -> Identity (GameState, [String])
callSetupStack gs = runStateT (execStateT setup gs) []
  
spec :: Spec
spec =
  describe "TwentyOneSpec" $ do
    winnerPhaseSpec
    drawTurnPatternSpec
    playerDrawingPhaseSpec
    newCardToPlayerSpec
    setupSpec
    
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
          result = runRand (
            runStateT (drawTurnPattern (\_ -> True) properPlayer (\_ _ _ -> initialGs)) initialGs
            ) ()
      result `shouldBe` ((sam, initialGs), ())
    it "Return: the expected player with the first card of the deck and an updated Gamestate if the exit condition is not met once" $ do
      let initialGs = gameState deck
          exitCond = (==51) . length . gameStateDeck
          updateStateFn = \gs p d -> gs {properPlayer=p, gameStateDeck=d}
          result = runRand (
            runStateT (drawTurnPattern exitCond properPlayer updateStateFn) initialGs
                           ) ()
          expectedPlayer = sam {hand=[Card {cValue = Two, cType = Clubs}]}
          expectedGs = (gameState (tail deck)) {properPlayer=expectedPlayer}
      result `shouldBe` ((expectedPlayer, expectedGs), ())

playerDrawingPhaseSpec :: Spec
playerDrawingPhaseSpec =
  describe "playerDrawingPhase" $ do
    let initialGs = gameState deck
    context "When the player in input has lost" $ do
      let lostPlayer = sam {hand=take 10 deck}
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
        updateGSSam = (properPlayer, (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'}))
        updateGSDealer = (dealerPlayer, (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'}))
    it "should add a new card to the player hands when called with player functions" $ do
      let newGamestate = callNewCardToPlayer (fst updateGSSam) (snd updateGSSam) initialGs
          expectedPlayer = sam {hand=[(head deck)]}
      properPlayer newGamestate `shouldBe` expectedPlayer
    it "should add a new card to the dealer hands when called with player functions" $ do
      let newGamestate = callNewCardToPlayer (fst updateGSDealer) (snd updateGSDealer) initialGs
          expectedPlayer = dealer {hand=[(head deck)]}
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
      let blackJackPlayer = sam {hand=[Card {cValue = Ace, cType = Clubs}, Card {cValue = Queen, cType = Hearts}]}
          initialFailGs = (gameState deck) {properPlayer= blackJackPlayer}
      it "should always print the blackjack expected message to stdout" $ do
        let stack = (snd . runIdentity. callSetupStack) initialFailGs
        length stack `shouldBe` 2
        (foldl (||) True . map (isInfixOf "BLACKJACK!!")) stack `shouldBe` True
      it "should return the error" $ do
        let result = callSetup initialFailGs
        isLeft result `shouldBe` True
        fromLeft undefined result `shouldBe` (userError . show) Blackjack
      
main :: IO ()
main = hspec spec