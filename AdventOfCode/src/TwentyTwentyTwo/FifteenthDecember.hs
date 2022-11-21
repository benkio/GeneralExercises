module TwentyTwentyTwo.FifteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/15December.txt"

testInput :: [Int]
testInput = undefined

fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = undefined

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = undefined
