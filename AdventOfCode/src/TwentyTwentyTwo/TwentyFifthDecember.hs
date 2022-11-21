module TwentyTwentyTwo.TwentyFifthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/25December.txt"

testInput :: [Int]
testInput = undefined

twentyFifthDecemberSolution1 :: IO Int
twentyFifthDecemberSolution1 = undefined

twentyFifthDecemberSolution2 :: IO Int
twentyFifthDecemberSolution2 = undefined
