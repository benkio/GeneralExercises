module IO.TwentyOne
    ( someFunc
    ) where

import Pure.Domain
import Pure.Rules
import Control.Monad.State.Lazy
import System.Random.Shuffle

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data GameState = GameState {
  properPlayer :: Player,
  dealerPlayer :: Player,
  d   :: Deck
  }

initialState :: StateT [Card] IO GameState
initialState = do
  samCards <- replicateM 2 drawACard
  dealerCards <- replicateM 2 drawACard
  d <- get
  return (GameState (sam {hand=samCards}) (dealer {hand=dealerCards}) d)

drawACard :: StateT [Card] IO Card
drawACard = do
  d <- get
  nnd <- if (null d) then lift (shuffleM deck) else return d
  put (tail nnd)
  return (head nnd)

-- gameLoop :: StateT GameState IO ()
-- gameLoop = do
--   gs <- get
  