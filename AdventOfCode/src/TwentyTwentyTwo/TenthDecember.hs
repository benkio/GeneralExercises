module TwentyTwentyTwo.TenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/10December.txt"

testInput :: [Int]
testInput = undefined

tenthDecemberSolution1 :: IO Int
tenthDecemberSolution1 = undefined

tenthDecemberSolution2 :: IO Int
tenthDecemberSolution2 = undefined
