{-# LANGUAGE FlexibleContexts #-}
module IO.TwentyOne where

import Pure.Domain
import Pure.Rules
import Pure.ControlFlow
import Control.Monad.State.Lazy
import Control.Monad.Random
import Control.Monad.Except
import Control.Exception
import Control.Exception.Base
import System.Random.Shuffle
import IO.Algebras
import Prelude hiding (putStrLn)

entryPoint :: (MonadIO m,
             MonadRandom m,
             MonadConsole m,
             MonadError IOException m) =>
  m ()
entryPoint = do
  sDeck <- (shuffleM deck)
  let gs = gameState sDeck
  game gs

game :: (MonadIO m,
         MonadRandom m,
         MonadConsole m,
         MonadError IOException m) =>
  GameState ->
  m ()
game initialGs = do
  gs <- execStateT initialState initialGs
  _ <- runStateT gameLoop gs
  return ()


initialState :: (MonadRandom mr,
                 MonadTrans (t),
                 Monad (t mr),
                 MonadState GameState (t mr)) =>
  t mr ()
initialState = do
  samCards <- replicateM 2 drawACard
  dealerCards <- replicateM 2 drawACard
  gs <- get
  let currentProperPlayer = (properPlayer gs) { hand=samCards }
      currentDealerPlayer = (dealerPlayer gs) { hand=dealerCards }
  put gs {properPlayer=currentProperPlayer, dealerPlayer=currentDealerPlayer}
  return ()

drawACard :: (MonadRandom mr,
              MonadTrans t,
              Monad (t mr),
              MonadState GameState (t mr)) =>
  t mr Card
drawACard = do
  gs <- get
  let d = gameStateDeck gs
  nnd <- if (null d) then lift (shuffleM deck) else return d
  put $ gs {gameStateDeck=tail nnd}
  return (head nnd)

gameLoop :: (MonadRandom mr,
             MonadTrans t,
             Monad (t mr),
             MonadState GameState (t mr),
             MonadError IOException (t mr),
              MonadConsole mr) =>
  t mr ()
gameLoop = catchError (
  do
    setup
    samP <- drawTurnPattern (hasMoreThen17 . properPlayer) properPlayer (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'})
    playerDrawingPhase samP
    dealerP <- drawTurnPattern (\gs -> (dealerPlayer gs) > (properPlayer gs)) dealerPlayer (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'})
    playerDrawingPhase dealerP
    winnerPhase
  ) (\err -> lift $ putStrLn "End of the Game")

setup  :: (MonadError IOException (t m),
           MonadState GameState (t m),
           MonadConsole m,
           MonadTrans t,
           Monad m) =>
          t m ()
setup = do
  gs <- get
  blackjacks <- lift $ mapM (\p -> return (hasBlackjack p) >>= \b -> putStrLn (blackjackMessage b p) >> return b) [properPlayer gs, dealerPlayer gs]
  if (foldr (||) False blackjacks)
    then (liftEither . Left . userError . show) Blackjack
    else liftEither $ Right ()


newCardToPlayer :: (MonadRandom mr,
                    MonadTrans t,
                    Monad (t mr),
                    MonadState GameState (t mr)) =>
  (GameState -> Player) ->
  (GameState -> Player -> Deck -> GameState) ->
  t mr ()
newCardToPlayer playerSelection updateGs = do
  nCard <- drawACard
  gs <- get
  let nDeck = gameStateDeck gs
  let newGs = let statePlayer = playerSelection gs
                  newStatePlayer = statePlayer {hand=nCard:(hand statePlayer)}
              in updateGs gs newStatePlayer nDeck
  put newGs
  return ()

playerDrawingPhase :: (MonadError IOException (t m),
                       MonadConsole m,
                       MonadTrans t,
                       Monad m) =>
                      Player -> t m ()
playerDrawingPhase playerState = do
  let playerLost = hasLost playerState
  lift $ putStrLn $ lostMessage playerLost playerState
  if playerLost
    then (liftEither . Left . userError . show) PlayerLost
    else liftEither $ Right ()

drawTurnPattern :: (MonadRandom mr,
                    MonadTrans t,
                    Monad (t mr),
                    MonadState GameState (t mr)) =>
  (GameState -> Bool) ->
  (GameState -> Player) ->
  (GameState -> Player -> Deck -> GameState) ->
  t mr Player
drawTurnPattern exitCondition playerExtraciton gameStateUpdate =
  do
    gs <- get
    if (exitCondition gs)
    then return (playerExtraciton gs)
    else newCardToPlayer playerExtraciton gameStateUpdate >>
         drawTurnPattern exitCondition playerExtraciton gameStateUpdate

winnerPhase :: (MonadTrans t,
                Monad (t mr),
                Monad mr,
                MonadConsole mr,
                MonadState GameState (t mr)) =>
  t mr ()
winnerPhase = do
  gs <- get
  lift $ putStrLn $ pickAWinner gs
  return ()