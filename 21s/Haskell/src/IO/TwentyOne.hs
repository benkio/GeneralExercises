module IO.TwentyOne
    ( someFunc
    ) where

import Pure.Domain
import Pure.Rules
import Control.Monad.State.Lazy
import Control.Monad.Error.Class
import Control.Applicative
import System.Random.Shuffle

someFunc :: IO ()
someFunc = shuffleM deck >>= game

data GameState = GameState {
  properPlayer :: Player,
  dealerPlayer :: Player,
  gameStateDeck   :: Deck
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

newCardToPlayer :: GameState -> (GameState -> Player) -> (GameState -> Player -> Deck -> GameState) -> StateT GameState IO ()
newCardToPlayer gs playerSelection updateGs = do
  (nCard, nDeck) <- liftIO $ runStateT drawACard (gameStateDeck gs)
  let newGs = let statePlayer = playerSelection gs
                  newStatePlayer = statePlayer {hand=nCard:(hand statePlayer)}
              in updateGs gs newStatePlayer nDeck
  put newGs
  return ()

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

gameLoop :: StateT GameState IO ()
gameLoop = catchError (do
  setup
  playerDrawingPhase samDrawTurn 
  playerDrawingPhase dealerDrawTurn 
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
  checkSam <- lift $ checkBlackjack $ properPlayer gs
  checkDealer <- lift $ checkBlackjack $ dealerPlayer gs
  if (checkSam || checkDealer)
  then throwError $ error "blackjack"
  else return ()


playerDrawingPhase :: StateT GameState IO Player -> StateT GameState IO ()
playerDrawingPhase playerDrawTurn = do
  playerState <- playerDrawTurn
  checkPlayer <- lift $ checkLost $ playerState
  if (checkPlayer)
    then throwError $ error "player lost"
    else return ()

samDrawTurn :: StateT GameState IO Player
samDrawTurn = do
  gs <- get
  if (hasMoreThen17 (properPlayer gs))
    then return (properPlayer gs)
    else do
    newCardToPlayer gs properPlayer (\gst p d' -> gst {properPlayer=p, gameStateDeck=d'})
    samDrawTurn

dealerDrawTurn :: StateT GameState IO Player
dealerDrawTurn = do
  gs <- get
  if ((dealerPlayer gs) > (properPlayer gs))
    then return (dealerPlayer gs)
    else do
    newCardToPlayer gs dealerPlayer (\gst p d' -> gst {dealerPlayer=p, gameStateDeck=d'})
    dealerDrawTurn

winnerPhase :: StateT GameState IO ()
winnerPhase = do
  gs <- get
  let dealer = dealerPlayer gs
  let dScore = show (score dealer)
  let sam = properPlayer gs
  let sScore = show (score sam)
  liftIO $
    case (compare dealer sam) of GT -> putStrLn $ "The dealer win the game: " ++ dScore ++ " over " ++ sScore
                                 EQ -> putStrLn $ "Tie game at " ++ dScore
                                 LT -> putStrLn $ (name sam) ++ " wins the game: " ++ sScore ++ " over " ++ dScore
  return ()