module TwentyTwentyTwo.SeventeenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/17December.txt"

testInput :: [Int]
testInput = undefined

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = undefined

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = undefined
