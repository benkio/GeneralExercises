module TwentyTwentyThree.FourteenthDecember where

import Data.List (group, transpose)
import Data.Maybe (fromMaybe, isJust, isNothing)

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

solution2 :: Int -> Platform -> Platform
solution2 i = (!! i) . iterate tiltCycle

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 = undefined

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse
