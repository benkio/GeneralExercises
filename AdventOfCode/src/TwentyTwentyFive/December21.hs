module TwentyTwentyFive.December21 where

input :: IO a
input = parseInput <$> readFile "input/2025/December21.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december21Solution1 :: IO Int
december21Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december21Solution2 :: IO Int
december21Solution2 = solution2 <$> input
