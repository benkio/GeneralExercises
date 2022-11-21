module TwentyTwentyTwo.NinthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/9December.txt"

testInput :: [Int]
testInput = undefined

ninthDecemberSolution1 :: IO Int
ninthDecemberSolution1 = undefined

ninthDecemberSolution2 :: IO Int
ninthDecemberSolution2 = undefined
