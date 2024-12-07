module TwentyTwentyFour.December07 where

import Data.Tree (Tree, unfoldTree)

data Equation = E
    { test :: Int
    , terms :: [Int]
    }
    deriving (Show)
type Operator = Int -> Int -> Int

input :: IO [Equation]
input = parseInput <$> readFile "input/2024/December07.txt"

parseInput :: String -> [Equation]
parseInput = fmap parseEq . lines
  where
    parseEq :: String -> Equation
    parseEq = (\ls -> E{test = ((\x -> read x :: Int) . init . head) ls, terms = (fmap (\x -> read x :: Int) . tail) ls}) <$> words

operators :: [Operator]
operators = [(*), (+)]

testEq :: Equation -> Bool
testEq (E{test = t, terms = ts}) = elem t $ computeTerms ts

computeTerms :: [Int] -> [Int]
computeTerms [] = [0]
computeTerms [a] = [a]
computeTerms (a : b : xs) = operators >>= (\ x -> computeTerms (x : xs)) . (\ o -> o a b)

testInput :: [Equation]
testInput =
    parseInput
        "190: 10 19\n\
        \3267: 81 40 27\n\
        \83: 17 5\n\
        \156: 15 6\n\
        \7290: 6 8 6 15\n\
        \161011: 16 10 13\n\
        \192: 17 8 14\n\
        \21037: 9 7 18 13\n\
        \292: 11 6 16 20"

solution1 :: [Equation] -> Int
solution1 = sum . fmap test . filter testEq

december07Solution1 :: IO Int
december07Solution1 = solution1 <$> input

solution2 :: [Equation] -> Int
solution2 = undefined

december07Solution2 :: IO Int
december07Solution2 = solution2 <$> input
