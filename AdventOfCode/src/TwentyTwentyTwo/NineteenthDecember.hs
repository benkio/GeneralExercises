module TwentyTwentyTwo.NineteenthDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/19December.txt"

testInput :: [Int]
testInput = undefined

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
