module TwentyTwentyTwo.FourteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/14December.txt"

testInput :: [Int]
testInput = undefined

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 = undefined

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 = undefined
