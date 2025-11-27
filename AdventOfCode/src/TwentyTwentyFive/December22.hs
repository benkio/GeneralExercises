module TwentyTwentyFive.December22 where

input :: IO a
input = parseInput <$> readFile "input/2025/December22.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december22Solution1 :: IO Int
december22Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december22Solution2 :: IO Int
december22Solution2 = solution2 <$> input
