module TwentyTwentyTwo.EleventhDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2022/11December.txt"

testInput :: [Int]
testInput = undefined

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
