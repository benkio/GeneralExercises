module TwentyTwentyTwo.TwelfthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/12December.txt"

testInput :: [Int]
testInput = undefined

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = undefined

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = undefined
