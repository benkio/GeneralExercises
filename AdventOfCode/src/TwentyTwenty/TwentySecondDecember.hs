-------------------------------------------------------------------------------
--                           Advent Of Code - day 22                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentySecondDecember where

import           Data.Bifunctor

input :: IO (Player, Player)
input = parsePlayersDecks <$> readFile "input/2020/22December.txt"

data Player =
  Player Int [Int]
  deriving (Show, Eq)

playerId :: Player -> Int
playerId (Player x _) = x

playerDeck :: Player -> [Int]
playerDeck (Player _ x) = x

infiniteLoopInputTest :: (Player, Player)
infiniteLoopInputTest =
  parsePlayersDecks
    "Player 1:\n\
\43\n\
\19\n\
\\n\
\Player 2:\n\
\2\n\
\29\n\
\14"

inputTest :: (Player, Player)
inputTest =
  parsePlayersDecks
    "Player 1:\n\
\9\n\
\2\n\
\6\n\
\3\n\
\1\n\
\\n\
\Player 2:\n\
\5\n\
\8\n\
\4\n\
\7\n\
\10"

parsePlayersDecks :: String -> (Player, Player)
parsePlayersDecks =
  bimap parseSinglePlayer (parseSinglePlayer . tail) . break ("" ==) . lines
  where
    parseSinglePlayer :: [String] -> Player
    parseSinglePlayer s =
      Player
        (((\x -> read [x] :: Int) . head . tail . dropWhile (' ' /=) . head) s)
        ((fmap (\x -> read x :: Int) . tail) s)

combat :: Player -> Player -> Player
combat (Player _ []) x = x
combat x (Player _ []) = x
combat p1@(Player x1 (c1:cs1)) p2@(Player x2 (c2:cs2))
  | c1 > c2 = combat (Player x1 (cs1 ++ [c1, c2])) (Player x2 cs2)
  | c2 > c1 = combat (Player x1 cs1) (Player x2 (cs2 ++ [c2, c1]))
  | otherwise = error $ "No draw allowed " ++ show p1 ++ " " ++ show p2

calculateScore :: (Player -> Player -> Player) -> (Player, Player) -> Int
calculateScore game =
  sum . (\(Player _ xs) -> zipWith (*) (reverse xs) [1 ..]) . uncurry game

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = calculateScore combat <$> input

recursiveCombatStep1 ::
     [(Player, Player)] -> (Player, Player) -> (Player, Player)
recursiveCombatStep1 historyStack (p1, p2)
  | (p1, p2) `elem` historyStack = (p1, p2Empty)
  | otherwise = recursiveCombatStep2 historyStack (p1, p2)
  where
    p2Empty = Player (playerId p2) []

recursiveCombatStep2 ::
     [(Player, Player)] -> (Player, Player) -> (Player, Player)
recursiveCombatStep2 historyStack x@(Player _ [], _) = x
recursiveCombatStep2 historyStack x@(_, Player _ []) = x
recursiveCombatStep2 historyStack s@(Player x1 (c1:cs1), Player x2 (c2:cs2))
  | length cs2 >= c2 && length cs1 >= c1 =
    let (rp1, rp2) =
          recursiveCombatStep1
            []
            (Player x1 (take c1 cs1), Player x2 (take c2 cs2))
     in if null (playerDeck rp1)
          then player2Wins
          else player1Wins
  | c1 > c2 = player1Wins
  | c2 > c1 = player2Wins
  | otherwise = error $ "No draw allowed " ++ show s
  where
    player2Wins =
      recursiveCombatStep1
        (historyStack ++ [s])
        (Player x1 cs1, Player x2 (cs2 ++ [c2, c1]))
    player1Wins =
      recursiveCombatStep1
        (historyStack ++ [s])
        (Player x1 (cs1 ++ [c1, c2]), Player x2 cs2)

solution2 :: (Player, Player) -> Int
solution2 = sum . reduceStateToWinner . recursiveCombatStep1 []
  where
    calculateScorePerCard :: [Int] -> [Int]
    calculateScorePerCard xs = zipWith (*) (reverse xs) [1 ..]
    reduceStateToWinner :: (Player, Player) -> [Int]
    reduceStateToWinner (Player _ [], Player _ xs) = calculateScorePerCard xs
    reduceStateToWinner (Player _ xs, Player _ []) = calculateScorePerCard xs

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = solution2 <$> input
