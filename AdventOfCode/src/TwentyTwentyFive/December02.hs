module TwentyTwentyFive.December02 where

input :: IO a
input = parseInput <$> readFile "input/2025/December02.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december02Solution1 :: IO Int
december02Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december02Solution2 :: IO Int
december02Solution2 = solution2 <$> input
