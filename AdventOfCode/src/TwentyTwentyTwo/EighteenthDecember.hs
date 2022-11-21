module TwentyTwentyTwo.EighteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/18December.txt"

testInput :: [Int]
testInput = undefined

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = undefined

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = undefined
