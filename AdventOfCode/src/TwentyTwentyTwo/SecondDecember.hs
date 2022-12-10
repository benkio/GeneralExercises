module TwentyTwentyTwo.SecondDecember where

import Data.Bifunctor (bimap, second)

data RPS = Rock | Paper | Scissors deriving (Show)

data GameOutcome = Win | Lose | Draw

outcomeToScore :: GameOutcome -> Int
outcomeToScore Win = 6
outcomeToScore Lose = 0
outcomeToScore Draw = 3

rpsToScore :: RPS -> Int
rpsToScore Rock = 1
rpsToScore Paper = 2
rpsToScore Scissors = 3

playRPS :: RPS -> RPS -> GameOutcome
playRPS Rock Paper = Win
playRPS Rock Scissors = Lose
playRPS Paper Scissors = Win
playRPS Paper Rock = Lose
playRPS Scissors Rock = Win
playRPS Scissors Paper = Lose
playRPS _ _ = Draw

playScoreRPS :: RPS -> RPS -> Int
playScoreRPS rps rps' =
    let gameScore = outcomeToScore $ playRPS rps rps'
        rpsScore = rpsToScore rps'
     in gameScore + rpsScore

stringToRPS :: String -> RPS
stringToRPS s
    | s == "A" || s == "X" = Rock
    | s == "B" || s == "Y" = Paper
    | s == "C" || s == "Z" = Scissors

input :: IO [String]
input = lines <$> readFile "input/2022/2December.txt"

parseInput :: [String] -> [(RPS, RPS)]
parseInput = fmap (bimap stringToRPS (stringToRPS . tail) . break (== ' '))

testInput :: String
testInput =
    "A Y\n\
    \B X\n\
    \C Z"

solution1 :: [(RPS, RPS)] -> Int
solution1 = foldl (\score (rps, rps') -> score + playScoreRPS rps rps') 0

secondDecemberSolution1 :: IO Int
secondDecemberSolution1 = solution1 . parseInput <$> input

stringToGameOutcome :: String -> GameOutcome
stringToGameOutcome "X" = Lose
stringToGameOutcome "Y" = Draw
stringToGameOutcome "Z" = Win

parseInput' :: [String] -> [(RPS, GameOutcome)]
parseInput' = fmap (bimap stringToRPS (stringToGameOutcome . tail) . break (== ' '))

whatToPlay :: RPS -> GameOutcome -> RPS
whatToPlay Rock Win = Paper
whatToPlay Rock Lose = Scissors
whatToPlay Paper Win = Scissors
whatToPlay Paper Lose = Rock
whatToPlay Scissors Win = Rock
whatToPlay Scissors Lose = Paper
whatToPlay rps Draw = rps

solution2 :: [(RPS, GameOutcome)] -> Int
solution2 = foldl (\score (rps, go) -> score + outcomeToScore go + rpsToScore (whatToPlay rps go)) 0

secondDecemberSolution2 :: IO Int
secondDecemberSolution2 = solution2 . parseInput' <$> input
