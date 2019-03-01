{-# LANGUAGE FlexibleContexts #-}
module IO.TwentyOne where

import Pure.Domain
import Pure.Rules
import Pure.ControlFlow
import Control.Monad.State.Lazy
import Control.Monad.Random
import Control.Monad.Error.Class
import Control.Applicative
import System.Random.Shuffle


someFunc :: MonadRandom m => m ()
someFunc = shuffleM deck >>= game

game :: (Monad m, MonadRandom m) => [Card] -> m ()
game d = do
  gs <- evalStateT initialState d
  _ <- execStateT gameLoop gs
  return ()


initialState :: (MonadRandom mr,
              MonadTrans (m [Card]),
              Monad (m [Card] mr),
              MonadState [Card] (m [Card] mr)) => m [Card] mr GameState
initialState = do
  samCards <- replicateM 2 drawACard
  dealerCards <- replicateM 2 drawACard
  d <- get
  return (GameState (sam {hand=samCards}) (dealer {hand=dealerCards}) d)

drawACard :: (MonadRandom mr,
              MonadTrans (m [Card]),
              Monad (m [Card] mr),
              MonadState [Card] (m [Card] mr)) => m [Card] mr Card
drawACard = do
  d <- get
  nnd <- if (null d) then lift (shuffleM deck) else return d
  put (tail nnd)
  return (head nnd)

gameLoop :: StateT GameState IO ()
gameLoop = catchError (do
  setup
  playerDrawingPhase $ drawTurnPattern (hasMoreThen17 . properPlayer) properPlayer (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'})
  playerDrawingPhase $ drawTurnPattern (\gs -> (dealerPlayer gs) > (properPlayer gs)) dealerPlayer (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'})
  winnerPhase
  ) (\err -> liftIO $ putStrLn "End of the Game")

setup :: (Monad m,
          MonadIO (ms GameState m),
          MonadState GameState (ms GameState m),
          MonadError e0 (ms GameState m)) => ms GameState m ()
setup = do
  gs <- get
  blackjacks <- liftIO $ mapM (\p -> return (hasBlackjack p) >>= \b -> putStrLn (blackjackMessage b p) >> return b) [properPlayer gs, dealerPlayer gs]
  if (foldr (||) False blackjacks) then throwError $ error "blackjack"  else return ()


newCardToPlayer :: GameState -> (GameState -> Player) -> (GameState -> Player -> Deck -> GameState) -> StateT GameState IO ()
newCardToPlayer gs playerSelection updateGs = do
  nCard <- drawACard (gameStateDeck gs)
  nDeck <- get
  let newGs = let statePlayer = playerSelection gs
                  newStatePlayer = statePlayer {hand=nCard:(hand statePlayer)}
              in updateGs gs newStatePlayer nDeck
  put newGs
  return ()

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