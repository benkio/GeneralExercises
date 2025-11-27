module TwentyTwentyFive.December16 where

input :: IO a
input = parseInput <$> readFile "input/2025/December16.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december16Solution1 :: IO Int
december16Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december16Solution2 :: IO Int
december16Solution2 = solution2 <$> input
