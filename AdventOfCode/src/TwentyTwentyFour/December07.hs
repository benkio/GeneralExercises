module TwentyTwentyFour.December07 where

input :: IO a
input = parseInput <$> readFile "input/2024/December07.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december07Solution1 :: IO Int
december07Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december07Solution2 :: IO Int
december07Solution2 = solution2 <$> input

