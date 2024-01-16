{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.FourteenthDecember where

import Data.List (group, transpose)
import Data.Map (Map, empty, insert)
import qualified Data.Map as M (lookup)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Debug.Trace

type Platform = [String]

input :: IO Platform
input = parseInput <$> readFile "input/2023/14December.txt"

parseInput = lines

testInput :: Platform
testInput =
    parseInput
        "O....#....\n\
        \O.OO#....#\n\
        \.....##...\n\
        \OO.#O....O\n\
        \.O.....O#.\n\
        \O.#..O.#.#\n\
        \..O..#O..O\n\
        \.......O..\n\
        \#....###..\n\
        \#OO..#...."

tiltHorizontal :: Bool -> String -> String
tiltHorizontal tiltWest s = (\(s, mrrs) -> if tiltWest then fromMaybe "" mrrs ++ s else s ++ fromMaybe "" mrrs) $ tiltFunction ("", Nothing) $ group s
  where
    tiltFunction = if tiltWest then foldr moveRoundedRocksLeft else foldl moveRoundedRocksRight
    moveRoundedRocksLeft :: String -> (String, Maybe String) -> (String, Maybe String)
    moveRoundedRocksLeft s (acc, mrrs)
        | all (== 'O') s && isNothing mrrs = (acc, Just s)
        | all (== 'O') s && isJust mrrs = (acc, fmap (++ s) mrrs)
        | all (== '.') s = (s ++ acc, mrrs)
        | all (== '#') s = (s ++ fromMaybe "" mrrs ++ acc, Nothing)
    moveRoundedRocksRight :: (String, Maybe String) -> String -> (String, Maybe String)
    moveRoundedRocksRight (acc, mrrs) s
        | all (== 'O') s && isNothing mrrs = (acc, Just s)
        | all (== 'O') s && isJust mrrs = (acc, fmap (++ s) mrrs)
        | all (== '.') s = (acc ++ s, mrrs)
        | all (== '#') s = (acc ++ fromMaybe "" mrrs ++ s, Nothing)

tiltNorth :: Platform -> Platform
tiltNorth = rotateRight . fmap (tiltHorizontal True) . rotateLeft
tiltWest :: Platform -> Platform
tiltWest = fmap (tiltHorizontal True)
tiltEst :: Platform -> Platform
tiltEst = fmap (tiltHorizontal False)
tiltSouth :: Platform -> Platform
tiltSouth = rotateLeft . fmap (tiltHorizontal True) . rotateRight

tiltCycle :: Platform -> Platform
tiltCycle = tiltEst . tiltSouth . tiltWest . tiltNorth

calculateLoad :: String -> Int
calculateLoad s = sum $ zipWith load (reverse [1 .. (length s)]) s
  where
    load :: Int -> Char -> Int
    load i 'O' = i
    load _ _ = 0

solution1 = sum . fmap (calculateLoad . tiltHorizontal True) . rotateLeft

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 = solution1 <$> input

cycleLoop :: Int -> Platform -> Platform
cycleLoop i = (!! i) . iterate tiltCycle

-- return when the repetition starts and ends
findRepetition :: Platform -> (Int, Int)
findRepetition = go 0 empty
  where
    platformToKey = concat
    go :: Int -> Map String Int -> Platform -> (Int, Int)
    go i m p =
        let p' = tiltCycle p
            i' = i + 1
         in maybe (go i' (insert (platformToKey p') i' m) p') ((,i')) $ M.lookup (platformToKey p') m

solution2 :: Platform -> [Int]
solution2 p =
    -- solution1 endPlatform
    (fmap solution1 . take 50) (iterate tiltCycle p)
  where
    (startRep, endRep) = (\(a, b) -> (a - 1, b - 1)) $ findRepetition p
    cycleLength = endRep - startRep
    remainingCycles = (1000000000 - endRep) `mod` cycleLength
    endPlatform = cycleLoop (traceShowId remainingCycles) p

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 = undefined

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse
