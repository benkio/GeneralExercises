module IO.TwentyOneSpec where

import Pure.Domain
import Test.Hspec
import Test.QuickCheck
import Control.Monad.State.Lazy
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
spec = winnerPhaseSpec

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
   



main :: IO ()
main = hspec spec