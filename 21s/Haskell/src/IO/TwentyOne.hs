module IO.TwentyOne
    ( someFunc
    ) where

import Pure.Domain
import Pure.Rules
import Control.Monad.State.Lazy
import Control.Applicative
import System.Random.Shuffle

someFunc :: IO ()
someFunc = game deck

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

checkBlackjack :: Player -> IO Bool
checkBlackjack p = if (hasBlackjack p)
    then putStrLn ( name p ++ " BLACKJACK!!") >>
         putStrLn ( "Hand of " ++ name p ++ ": " ++ show (hand p)) >>
         return True
    else putStrLn ( "Hand of " ++ name p ++ ": " ++ show (hand p)) >>
         return False

checkLost :: Player -> IO Bool
checkLost p = if (hasLost p)
  then putStrLn (name p ++ " HAS LOST!!!(" ++ show (score p) ++ ")") >>
       putStrLn ( "Hand of " ++ name p ++ ": " ++ show (hand p)) >>
       return True
  else putStrLn ( "Hand of " ++ name p ++ ": " ++ show (hand p)) >>
       return False

gameLoop :: StateT GameState IO (Maybe ())
gameLoop = setup <|>
           playerDrawingPhase samDrawTurn <|>
           playerDrawingPhase dealerDrawTurn <|>
           winnerPhase

game :: [Card] -> IO ()
game d = do
  gs <- evalStateT initialState d
  _ <- execStateT gameLoop gs
  return ()

-- return nothing if I want to continue otherwise return unit. I compose then all the phases with <|>

setup :: StateT GameState IO (Maybe ())
setup = do
  gs <- get
  checkSam <- lift $ checkBlackjack $ properPlayer gs
  checkDealer <- lift $ checkBlackjack $ dealerPlayer gs
  if (checkSam || checkDealer)
  then return (Just ())
  else return Nothing


playerDrawingPhase :: StateT GameState IO Player -> StateT GameState IO (Maybe ())
playerDrawingPhase playerDrawTurn = do
  playerState <- playerDrawTurn
  checkPlayer <- lift $ checkLost $ playerState
  if (checkPlayer)
    then return (Just ())
    else return Nothing

samDrawTurn :: StateT GameState IO Player
samDrawTurn = do
  gs <- get
  if (hasMoreThen17 (properPlayer gs))
    then return (properPlayer gs)
    else
    undefined --Draw a card and add it to the sam hand. update the state and do recursion

dealerDrawTurn :: StateT GameState IO Player
dealerDrawTurn = undefined

winnerPhase :: StateT GameState IO (Maybe ())
winnerPhase = undefined