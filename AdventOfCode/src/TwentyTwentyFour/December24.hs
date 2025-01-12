module TwentyTwentyFour.December24 where

input :: IO a
input = parseInput <$> readFile "input/2024/December24.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december24Solution1 :: IO Int
december24Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december24Solution2 :: IO Int
december24Solution2 = solution2 <$> input
