module TwentyTwentyTwo.TwentiethDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/20December.txt"

testInput :: [Int]
testInput = undefined

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = undefined

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = undefined
