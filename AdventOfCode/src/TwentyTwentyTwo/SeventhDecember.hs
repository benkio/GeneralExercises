module TwentyTwentyTwo.SeventhDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/7December.txt"

testInput :: [Int]
testInput = undefined

seventhDecemberSolution1 :: IO Int
seventhDecemberSolution1 = undefined

seventhDecemberSolution2 :: IO Int
seventhDecemberSolution2 = undefined
