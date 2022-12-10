module TwentyTwentyOne.TwentyFirstDecember where

import Control.Applicative
import Control.Monad.State.Lazy
import Data.Bifunctor (bimap, first)
import Data.List (find, group, sort, (\\))
import Data.Map (Map)
import qualified Data.Map as M (adjust, alter, empty, filter, foldrWithKey, fromList, insert, insertWith, keys, lookup, size, toList)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Debug.Trace

data GameState = GameState
    { players :: [Player]
    , dice :: [Int]
    }
    deriving (Show)

data Player = Player
    { playerId :: Int
    , position :: Int
    , score :: Int
    }
    deriving (Show, Eq, Ord)

input :: IO String
input = readFile "input/2021/21December.txt"

inputTest :: String
inputTest =
    "Player 1 starting position: 4\n\
    \Player 2 starting position: 8"

parseInput :: String -> GameState
parseInput i =
    let [p1s, p2s] = lines i
     in GameState
            { players =
                [ Player{playerId = 1, position = parsePlayer p1s, score = 0}
                , Player{playerId = 2, position = parsePlayer p2s, score = 0}
                ]
            , dice = buildDice 1
            }
  where
    parsePlayer = (\x -> read x :: Int) . drop 2 . dropWhile (/= ':')

board :: [Int]
board = cycle [1 .. 10]

buildDice :: Int -> [Int]
buildDice x = cycle $ [x .. 1000] ++ [1 .. (x - 1)]

moveOnBoard :: Int -> Int -> Int
moveOnBoard p x = (head . dropWhile (/= (if (p + x) `mod` 10 == 0 then 10 else rem (p + x) 10))) board

playerTurn :: Int -> State GameState ()
playerTurn pId = do
    gs <- get
    let p = fromJust . find ((== pId) . playerId) $ players gs
        rollValue = (sum . take 3) (dice gs)
        newPosition = moveOnBoard (position p) rollValue
        p' = p{position = newPosition, score = score p + newPosition}
    put (gs{dice = buildDice (dice gs !! 3), players = ((p' :) . filter ((/= pId) . playerId)) (players gs)})
    return ()

game :: Int -> State GameState (Int, Int)
game rolls = do
    playerTurn 1
    exit1 <- exitGame (rolls + 3)
    playerTurn 2
    exit2 <- exitGame (rolls + 6)
    let exit = exit1 <|> exit2
    maybe (game (rolls + 6)) return exit

exitGame :: Int -> State GameState (Maybe (Int, Int))
exitGame rolls = do
    gs <- get
    return $ if (any (>= 1000) . fmap score . players) gs then Just (rolls, (minimum . fmap score . players) gs) else Nothing

twentyFirstDecemberSolution1 :: IO Int
twentyFirstDecemberSolution1 = uncurry (*) . evalState (game 0) . parseInput <$> input

type GameState' = (Map ((Int, Int), (Int, Int)) Int, Map ((Int, Int), (Int, Int)) Int)

parseInput' :: String -> GameState'
parseInput' i =
    let [p1s, p2s] = lines i
     in initialGameState ((parsePlayer p1s, 0), (parsePlayer p2s, 0))
  where
    parsePlayer = (\x -> read x :: Int) . drop 2 . dropWhile (/= ':')

rollValues :: [(Int, Int)]
rollValues = (fmap (\x -> (head x, length x)) . group . sort) $ [x + y + z | x <- [3, 2, 1], y <- [3, 2, 1], z <- [3, 2, 1]]

playerStates :: [(Int, Int)]
playerStates = [(p, v) | p <- [1 .. 10], v <- [0 .. 20]]

gameStates :: Map ((Int, Int), (Int, Int)) Int
gameStates = M.fromList [((p1s, p2s), 0) | p1s <- playerStates, p2s <- playerStates]

initialGameState :: ((Int, Int), (Int, Int)) -> GameState'
initialGameState gs = (M.insert gs 1 gameStates, gameStates)

explodeState1 ::
    GameState' ->
    ((Int, Int), (Int, Int)) ->
    (Int, GameState')
explodeState1 (g1s, g2s) ps@(p1s, p2s) =
    let v = fromMaybe 0 $ M.lookup ps g1s
        np =
            filter ((> 0) . snd) $
                fmap
                    ( \(x, vx) ->
                        let p = moveOnBoard x (fst p1s)
                         in ((p, snd p1s + p), vx * v)
                    )
                    rollValues
        win1 = filter ((>= 21) . snd . fst) np
        g1s' = M.adjust (const 0) ps g1s
        g2s' = foldl (\m (p, vp) -> M.insertWith (+) (p, p2s) vp m) g2s (np \\ win1)
     in ((sum . fmap snd) win1, (g1s', g2s'))

explodeState2 ::
    GameState' ->
    ((Int, Int), (Int, Int)) ->
    (Int, GameState')
explodeState2 (g1s, g2s) ps@(p1s, p2s) =
    let v = fromMaybe 0 $ M.lookup ps g2s
        np =
            filter ((> 0) . snd) $
                fmap
                    ( \(x, vx) ->
                        let p = moveOnBoard x (fst p2s)
                         in ((p, snd p2s + p), vx * v)
                    )
                    rollValues
        win2 = filter ((>= 21) . snd . fst) np
        g2s' = M.adjust (const 0) ps g2s
        g1s' = foldl (\m (p, vp) -> M.insertWith (+) (p1s, p) vp m) g1s (np \\ win2)
     in ((sum . fmap snd) win2, (g1s', g2s'))

noMoreGames :: GameState' -> Bool
noMoreGames (gs1, gs2) = noMoreGamesP gs1 && noMoreGamesP gs2
  where
    noMoreGamesP = (== 0) . M.size . M.filter (> 0)

game' :: (Int, Int) -> GameState' -> Int
game' (v1, v2) gs@(gs1, gs2)
    | noMoreGames gs = max v1 v2
    | otherwise =
        let openGamesP1 = M.keys $ M.filter (> 0) gs1
            (w1, gs') = foldl (\(w, g) s -> first (+ w) $ explodeState1 g s) (0, gs) openGamesP1
            openGamesP2 = M.keys $ M.filter (> 0) gs2
            (w2, gs'') = foldl (\(w, g) s -> first (+ w) $ explodeState2 g s) (0, gs') openGamesP2
         in game' (v1 + w1, v2 + w2) gs''

twentyFirstDecemberSolution2 :: IO Int
twentyFirstDecemberSolution2 = game' (0, 0) . parseInput' <$> input
