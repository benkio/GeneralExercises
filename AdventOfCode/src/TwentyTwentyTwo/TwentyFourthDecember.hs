module TwentyTwentyTwo.TwentyFourthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/24December.txt"

testInput :: [Int]
testInput = undefined

twentyFourthDecemberSolution1 :: IO Int
twentyFourthDecemberSolution1 = undefined

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = undefined
