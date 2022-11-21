module TwentyTwentyTwo.TwentyThirdDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/23December.txt"

testInput :: [Int]
testInput = undefined

twentyThirdDecemberSolution1 :: IO Int
twentyThirdDecemberSolution1 = undefined

twentyThirdDecemberSolution2 :: IO Int
twentyThirdDecemberSolution2 = undefined
