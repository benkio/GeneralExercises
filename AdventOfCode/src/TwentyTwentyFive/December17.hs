module TwentyTwentyFive.December17 where

input :: IO a
input = parseInput <$> readFile "input/2025/December17.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december17Solution1 :: IO Int
december17Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december17Solution2 :: IO Int
december17Solution2 = solution2 <$> input
