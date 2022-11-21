module TwentyTwentyTwo.SecondDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/2December.txt"

testInput :: [Int]
testInput = undefined

secondDecemberSolution1 :: IO Int
secondDecemberSolution1 = undefined

secondDecemberSolution2 :: IO Int
secondDecemberSolution2 = undefined
