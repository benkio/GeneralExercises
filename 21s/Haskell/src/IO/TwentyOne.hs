module IO.TwentyOne
    ( someFunc
    ) where

import Pure.Domain
import Pure.Rules
import Pure.ControlFlow
import Control.Monad.State.Lazy
import Control.Monad.Error.Class
import Control.Applicative
import System.Random.Shuffle


someFunc :: IO ()
someFunc = shuffleM deck >>= game

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

newCardToPlayer :: GameState -> (GameState -> Player) -> (GameState -> Player -> Deck -> GameState) -> StateT GameState IO ()
newCardToPlayer gs playerSelection updateGs = do
  (nCard, nDeck) <- liftIO $ runStateT drawACard (gameStateDeck gs)
  let newGs = let statePlayer = playerSelection gs
                  newStatePlayer = statePlayer {hand=nCard:(hand statePlayer)}
              in updateGs gs newStatePlayer nDeck
  put newGs
  return ()

gameLoop :: StateT GameState IO ()
gameLoop = catchError (do
  setup
  playerDrawingPhase $ drawTurnPattern (hasMoreThen17 . properPlayer) properPlayer (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'})
  playerDrawingPhase $ drawTurnPattern (\gs -> (dealerPlayer gs) > (properPlayer gs)) dealerPlayer (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'})
  winnerPhase
  ) (\err -> liftIO $ putStrLn "End of the Game")

game :: [Card] -> IO ()
game d = do
  gs <- evalStateT initialState d
  _ <- execStateT gameLoop gs
  return ()

-- return nothing if I want to continue otherwise return unit. I compose then all the phases with <|>

setup :: StateT GameState IO ()
setup = do
  gs <- get
  blackjacks <- liftIO $ mapM (\p -> return (hasBlackjack p) >>= \b -> putStrLn (blackjackMessage b p) >> return b) [properPlayer gs, dealerPlayer gs]
  if (foldr (||) False blackjacks) then throwError $ error "blackjack"  else return ()


playerDrawingPhase :: StateT GameState IO Player -> StateT GameState IO ()
playerDrawingPhase playerDrawTurn = do
  playerState <- playerDrawTurn
  let playerLost = hasLost playerState
  lift $  putStrLn $ lostMessage playerLost playerState
  if playerLost
    then throwError $ error "player lost"
    else return ()

drawTurnPattern :: (GameState -> Bool) -> (GameState -> Player) -> (GameState -> Player -> Deck -> GameState) -> StateT GameState IO Player
drawTurnPattern exitCondition playerExtraciton gameStateUpdate = 
  do
    gs <- get
    if (exitCondition gs)
    then return (playerExtraciton gs)
    else newCardToPlayer gs dealerPlayer gameStateUpdate >>
         drawTurnPattern exitCondition playerExtraciton gameStateUpdate

winnerPhase :: StateT GameState IO ()
winnerPhase = do
  gs <- get
  liftIO $ putStrLn $ pickAWinner gs
  return ()