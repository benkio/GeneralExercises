module TwentyTwentyTwo.ThirteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/13December.txt"

testInput :: [Int]
testInput = undefined

thirteenthDecemberSolution1 :: IO Int
thirteenthDecemberSolution1 = undefined

thirteenthDecemberSolution2 :: IO Int
thirteenthDecemberSolution2 = undefined
