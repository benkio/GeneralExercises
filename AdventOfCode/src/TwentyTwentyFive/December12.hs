module TwentyTwentyFive.December12 where

input :: IO a
input = parseInput <$> readFile "input/2025/December12.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december12Solution1 :: IO Int
december12Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december12Solution2 :: IO Int
december12Solution2 = solution2 <$> input
