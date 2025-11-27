module TwentyTwentyFive.December13 where

input :: IO a
input = parseInput <$> readFile "input/2025/December13.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december13Solution1 :: IO Int
december13Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december13Solution2 :: IO Int
december13Solution2 = solution2 <$> input
