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


someFunc :: (MonadIO m,
             MonadRandom m,
             MonadError e00 m) =>
  m ()
someFunc = do
  sDeck <- (shuffleM deck)
  let gs = gameState sDeck
  game gs

game :: (MonadIO m,
         MonadRandom m,
         MonadError e00 m) =>
  GameState ->
  m ()
game initialGs = do
  gs <- execStateT initialState initialGs
  _ <- runStateT gameLoop gs
  return ()


initialState :: (MonadRandom mr,
                 MonadTrans (m GameState),
                 Monad (m GameState mr),
                 MonadState GameState (m GameState mr)) =>
  m GameState mr ()
initialState = do
  samCards <- replicateM 2 drawACard
  dealerCards <- replicateM 2 drawACard
  gs <- get
  let currentProperPlayer = (properPlayer gs) { hand=samCards }
      currentDealerPlayer = (dealerPlayer gs) { hand=dealerCards }
  put gs {properPlayer=currentProperPlayer, dealerPlayer=currentDealerPlayer}
  return ()

drawACard :: (MonadRandom mr,
              MonadTrans (m GameState),
              Monad (m GameState mr),
              MonadState GameState (m GameState mr)) =>
  m GameState mr Card
drawACard = do
  gs <- get
  let d = gameStateDeck gs
  nnd <- if (null d) then lift (shuffleM deck) else return d
  put $ gs {gameStateDeck=tail nnd}
  return (head nnd)

gameLoop :: (MonadRandom mr,
              MonadTrans (m GameState),
              MonadIO (m GameState mr),
              Monad (m GameState mr),
              MonadState GameState (m GameState mr),
              MonadError e0 (m GameState mr)) =>
  m GameState mr ()
gameLoop = catchError (do
  setup
  samP <- drawTurnPattern (hasMoreThen17 . properPlayer) properPlayer (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'})
  playerDrawingPhase samP
  dealerP <- drawTurnPattern (\gs -> (dealerPlayer gs) > (properPlayer gs)) dealerPlayer (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'})
  playerDrawingPhase dealerP
  winnerPhase
  ) (\err -> liftIO $ putStrLn "End of the Game")

setup :: (Monad m,
          MonadIO (ms GameState m),
          MonadState GameState (ms GameState m),
          MonadError e0 (ms GameState m)) =>
  ms GameState m ()
setup = do
  gs <- get
  blackjacks <- liftIO $ mapM (\p -> return (hasBlackjack p) >>= \b -> putStrLn (blackjackMessage b p) >> return b) [properPlayer gs, dealerPlayer gs]
  if (foldr (||) False blackjacks) then throwError $ error "blackjack"  else return ()


newCardToPlayer :: (MonadRandom mr,
                    MonadTrans (m GameState),
                    Monad (m GameState mr),
                    MonadState GameState (m GameState mr)) =>
  (GameState -> Player) ->
  (GameState -> Player -> Deck -> GameState) ->
  m GameState mr ()
newCardToPlayer playerSelection updateGs = do
  nCard <- drawACard
  gs <- get
  let nDeck = gameStateDeck gs
  let newGs = let statePlayer = playerSelection gs
                  newStatePlayer = statePlayer {hand=nCard:(hand statePlayer)}
              in updateGs gs newStatePlayer nDeck
  put newGs
  return ()

playerDrawingPhase :: (MonadRandom mr,
                       MonadTrans (m GameState),
                       MonadIO (m GameState mr),
                       Monad (m GameState mr),
                       MonadState GameState (m GameState mr),
                       MonadError e0 (m GameState mr)) =>
  Player ->
  m GameState mr ()
playerDrawingPhase playerState = do
  let playerLost = hasLost playerState
  liftIO $  putStrLn $ lostMessage playerLost playerState
  if playerLost
    then throwError $ error "player lost"
    else return ()

drawTurnPattern :: (MonadRandom mr,
                    MonadTrans (m GameState),
                    Monad (m GameState mr),
                    MonadState GameState (m GameState mr)) =>
  (GameState -> Bool) ->
  (GameState -> Player) ->
  (GameState -> Player -> Deck -> GameState) ->
  m GameState mr Player
drawTurnPattern exitCondition playerExtraciton gameStateUpdate =
  do
    gs <- get
    if (exitCondition gs)
    then return (playerExtraciton gs)
    else newCardToPlayer dealerPlayer gameStateUpdate >>
         drawTurnPattern exitCondition playerExtraciton gameStateUpdate

winnerPhase :: (MonadTrans (m GameState),
                MonadIO (m GameState mr),
                Monad (m GameState mr),
                MonadState GameState (m GameState mr)) =>
  m GameState mr ()
winnerPhase = do
  gs <- get
  liftIO $ putStrLn $ pickAWinner gs
  return ()