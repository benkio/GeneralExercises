module TwentyTwentyThree.December01 where

import Data.Char
import Data.Functor
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

december01Solution1 :: IO Int
december01Solution1 = solution1 <$> input

stringDigit :: [(String, Char)]
stringDigit = [("one", '1'), ("two", '2'), ("three", '3'), ("four", '4'), ("five", '5'), ("six", '6'), ("seven", '7'), ("eight", '8'), ("nine", '9')]

testInput2 =
    parseInput
        "two1nine\n\
        \eightwothree\n\
        \abcone2threexyz\n\
        \xtwone3four\n\
        \4nineeightseven2\n\
        \zoneight234\n\
        \7pqrstsixteen"

parsingStringDigit :: String -> Either (Char, String) String
parsingStringDigit xs = if null parsingList then Right (tail xs) else Left (head parsingList)
  where
    checkDigitString :: String -> (String, Char) -> Maybe (Char, String)
    checkDigitString xs (cs, i) = stripPrefix cs xs $> (i, tail xs)
    parsingList = mapMaybe (checkDigitString xs) stringDigit

parsingCalibration :: String -> [Char]
parsingCalibration [] = []
parsingCalibration (x : xs)
    | isDigit x = x : parsingCalibration xs
    | otherwise = case parsingStringDigit (x : xs) of
        Left (y, ys) -> y : parsingCalibration ys
        Right ys -> parsingCalibration ys

solution2 :: [String] -> Int
solution2 ls =
    sum $
        (\x -> read x :: Int) . (\x -> [head x, last x]) . parsingCalibration <$> ls

december01Solution2 :: IO Int
december01Solution2 = solution2 <$> input
