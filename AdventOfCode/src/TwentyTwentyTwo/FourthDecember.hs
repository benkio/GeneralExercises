module TwentyTwentyTwo.FourthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/4December.txt"

testInput :: [Int]
testInput = undefined

fourthDecemberSolution1 :: IO Int
fourthDecemberSolution1 = undefined

fourthDecemberSolution2 :: IO Int
fourthDecemberSolution2 = undefined
