module TwentyTwentyFive.December18 where

input :: IO a
input = parseInput <$> readFile "input/2025/December18.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december18Solution1 :: IO Int
december18Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december18Solution2 :: IO Int
december18Solution2 = solution2 <$> input
