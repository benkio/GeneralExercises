module TwentyTwentyTwo.ThirdDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/3December.txt"

testInput :: [Int]
testInput = undefined

thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = undefined

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 = undefined
