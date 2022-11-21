module TwentyTwentyTwo.TwentySecondDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/22December.txt"

testInput :: [Int]
testInput = undefined

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = undefined

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = undefined
