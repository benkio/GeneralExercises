module TwentyTwentyTwo.SixthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/6December.txt"

testInput :: [Int]
testInput = undefined

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = undefined

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = undefined
