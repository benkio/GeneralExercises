module TwentyTwentyTwo.SixteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/16December.txt"

testInput :: [Int]
testInput = undefined

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = undefined

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = undefined
