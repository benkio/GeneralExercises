module IO.TwentyOneSpec where

import Pure.Domain
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State.Lazy
import Control.Monad.Random.Lazy
import IO.EffectfulInstances
import IO.TwentyOne
import Data.List

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
callWinnerPhase gs = runState (do
  put []
  result <- execStateT winnerPhase gs
  return result) []

spec :: Spec
spec =
  describe "TwentyOneSpec" $ do
    winnerPhaseSpec
    drawTurnPatternSpec

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

main :: IO ()
main = hspec spec