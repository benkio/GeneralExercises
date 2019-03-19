module IO.TwentyOneSpec where

import Pure.Domain
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State.Lazy
import Control.Monad.Random.Lazy
import IO.EffectfulInstances
import IO.TwentyOne
import Data.List
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
callWinnerPhase gs = runIdentity $ runTestConsole [] $ callWinnerPhase' gs 

callWinnerPhase' :: GameState -> TestConsole Identity GameState
callWinnerPhase' gs = do
  put []
  result <- execStateT winnerPhase gs
  return result

callplayerDrawingState :: Player -> GameState -> TestConsole (TestError TestRandom) GameState
callplayerDrawingState p gs = execStateT (playerDrawingPhase p) gs

spec :: Spec
spec =
  describe "TwentyOneSpec" $ do
    winnerPhaseSpec
    drawTurnPatternSpec
    playerDrawingPhaseSpec
    
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
    context "When the player in input has lost" $
      it "Throw an error and add a println" $
        pending
    context "When the player in input is still in game" $
      it "Add a println and return ()" $
        pending

main :: IO ()
main = hspec spec