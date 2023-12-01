{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.FirstDecember where

import Data.Char
import Data.List
import Data.Maybe

input :: IO [String]
input = parseInput <$> readFile "input/2023/1December.txt"

parseInput :: String -> [String]
parseInput = lines

testInput :: [String]
testInput =
    parseInput
        "1abc2\n\
        \pqr3stu8vwx\n\
        \a1b2c3d4e5f\n\
        \treb7uchet"

solution1 :: [String] -> Int
solution1 ls = sum $ (\x -> read x :: Int) . (\x -> [head x, last x]) . filter isDigit <$> ls

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = solution1 <$> input

stringDigit :: [(String, Int)]
stringDigit = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]

parsingStringDigit :: String -> Either (Int, String) String
parsingStringDigit xs = if null parsingList then Right (tail xs) else Left (head parsingList)
  where
    checkDigitString :: String -> (String, Int) -> Maybe (String, Int)
    checkDigitString xs (cs, i) = (,i) <$> stripPrefix cs xs
    parsingList = mapMaybe xs stringDigit

parsingCalibration :: String -> [Int]
parsingCalibration (x : xs)
    | isDigit x = (read "x" :: Int) : parsingCalibration xs
    | otherwise = case parsingStringDigit (x : xs) of
        Left (y, ys) -> y : parsingCalibration ys
        Right ys -> parsingCalibration ys

solution2 = undefined

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = undefined
