module TwentyTwentyOne.EleventhDecember where

import Control.Monad.State.Lazy
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M (adjust, filter, fromList, keys, map, size)

input :: IO String
input = readFile "input/2021/11December.txt"

inputTest :: String
inputTest =
    "5483143223\n\
    \2745854711\n\
    \5264556173\n\
    \6141336146\n\
    \6357385478\n\
    \4167524645\n\
    \2176841721\n\
    \6882881134\n\
    \4846848554\n\
    \5283751526"

data Coord = Coord
    { x :: Int
    , y :: Int
    }
    deriving (Show, Ord, Eq)

type Cavern = Map Coord Int

parseInput :: String -> Cavern
parseInput =
    M.fromList
        . concatMap (\(a, r) -> (fmap (\(b, v) -> (Coord{x = b, y = a}, read [v] :: Int)) . zip [0 ..]) r)
        . zip [0 ..]
        . lines

neighboorsCoords :: Coord -> [Coord]
neighboorsCoords Coord{x = x', y = y'} =
    [Coord{x = a, y = b} | a <- [(x' - 1) .. (x' + 1)], b <- [(y' - 1) .. (y' + 1)], a /= x' || b /= y']

increaseEnergy :: Cavern -> Cavern
increaseEnergy = M.map (+ 1)

increaseByFlash :: [Coord] -> Cavern -> Cavern
increaseByFlash cs cavern =
    let neighboorsIncrease = concatMap neighboorsCoords cs
        -- set cs values to 0
        stopCurrentFlash = foldl' (flip (M.adjust (const 0))) cavern cs
     in -- set nneighboorsIncrease + 1
        foldl'
            ( flip
                ( M.adjust
                    ( \v ->
                        if v == 0 || v == 10 then v else v + 1
                    )
                )
            )
            stopCurrentFlash
            neighboorsIncrease

cicle :: State Cavern Int
cicle = state (stepFlashes . increaseEnergy)

stepFlashes :: Cavern -> (Int, Cavern)
stepFlashes cavern =
    let currentFlashCoords = M.keys $ M.filter (== 10) cavern
        currentFlash = length currentFlashCoords
     in if currentFlash == 0
            then (0, cavern)
            else
                let cavern' = increaseByFlash currentFlashCoords cavern
                    (neighboorsFlash, cavern'') = stepFlashes cavern'
                 in (currentFlash + neighboorsFlash, cavern'')

solution1 :: Int -> Cavern -> Int
solution1 0 _ = 0
solution1 steps c =
    let (f, c') = runState cicle c
     in f + solution1 (steps - 1) c'

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = solution1 100 . parseInput <$> input

solution2 :: Int -> Cavern -> Int
solution2 step c =
    let (f, c') = runState cicle c
     in if f == M.size c' then step + 1 else solution2 (step + 1) c'

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = solution2 0 . parseInput <$> input
