module TwentyTwentyOne.ThirdDecember where

import Data.List (group, maximumBy, minimumBy, sort, transpose)
import Debug.Trace (traceShow, traceShowId)

input :: IO [String]
input = lines <$> readFile "input/2021/3December.txt"

testInput :: [String]
testInput =
  lines
    "00100\n\
    \11110\n\
    \10110\n\
    \10111\n\
    \10101\n\
    \01111\n\
    \00111\n\
    \11100\n\
    \10000\n\
    \11001\n\
    \00010\n\
    \01010"

calculateEpsilon :: String -> String
calculateEpsilon [] = []
calculateEpsilon ('0' : xs) = '1' : calculateEpsilon xs
calculateEpsilon ('1' : xs) = '0' : calculateEpsilon xs

toDecimal :: String -> Int
toDecimal = sum . zipWith digitToNum [0 ..] . reverse
  where
    digitToNum :: Int -> Char -> Int
    digitToNum index c = (read [c] :: Int) * 2 ^ index

findGamma :: ([(Char, Int)] -> (Char, Int)) -> [String] -> String
findGamma maxLogic xs =
  fst
    . maxLogic
    . fmap (\s -> (head s, length s))
    . group
    . sort
    <$> transpose xs

solution1 :: [String] -> Int
solution1 xs =
  let gamma = findGamma (maximumBy (\t t' -> snd t `compare` snd t')) xs
      epsilon = calculateEpsilon gamma
   in toDecimal gamma * toDecimal epsilon

thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = solution1 <$> input

oxygenBitCriteria :: (Char, Int) -> (Char, Int) -> Ordering
oxygenBitCriteria t t'
  | snd t > snd t' = GT
  | snd t < snd t' = LT
  | fst t == '1' = GT
  | fst t' == '1' = LT

dioxideBitCriteria :: (Char, Int) -> (Char, Int) -> Ordering
dioxideBitCriteria t t'
  | snd t > snd t' = GT
  | snd t < snd t' = LT
  | fst t == '0' = LT
  | fst t' == '0' = GT

findVariable :: Int -> ([String] -> String) -> [String] -> String
findVariable _ _ [] = []
findVariable _ _ [x] = x
findVariable i fg xs =
  let g = fg xs
      xs' = filter (\x -> x !! i == g !! i) xs
   in findVariable (i + 1) fg xs'

solution2 :: [String] -> Int
solution2 xs =
  let oxigen = findVariable 0 (findGamma (maximumBy oxygenBitCriteria)) xs
      dioxide = findVariable 0 (findGamma (minimumBy dioxideBitCriteria)) xs
   in toDecimal oxigen * toDecimal dioxide

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 = solution2 <$> input
