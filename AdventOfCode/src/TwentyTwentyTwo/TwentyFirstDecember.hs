module TwentyTwentyTwo.TwentyFirstDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/21December.txt"

testInput :: [Int]
testInput = undefined

twentyFirstDecemberSolution1 :: IO Int
twentyFirstDecemberSolution1 = undefined

twentyFirstDecemberSolution2 :: IO Int
twentyFirstDecemberSolution2 = undefined
