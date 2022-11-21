module TwentyTwentyTwo.FifthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/5December.txt"

testInput :: [Int]
testInput = undefined

fifthDecemberSolution1 :: IO Int
fifthDecemberSolution1 = undefined

fifthDecemberSolution2 :: IO Int
fifthDecemberSolution2 = undefined
