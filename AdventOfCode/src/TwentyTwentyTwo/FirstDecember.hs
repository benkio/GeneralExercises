module TwentyTwentyTwo.FirstDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/1December.txt"

testInput :: [Int]
testInput = undefined

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = undefined

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = undefined
