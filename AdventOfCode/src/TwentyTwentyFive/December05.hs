module TwentyTwentyFive.December05 where

input :: IO a
input = parseInput <$> readFile "input/2025/December05.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december05Solution1 :: IO Int
december05Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december05Solution2 :: IO Int
december05Solution2 = solution2 <$> input
