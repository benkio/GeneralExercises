module TwentyTwentyTwo.EighthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/8December.txt"

testInput :: [Int]
testInput = undefined

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = undefined

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = undefined
