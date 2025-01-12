module TwentyTwentyFour.December25 where

input :: IO a
input = parseInput <$> readFile "input/2024/December25.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december25Solution1 :: IO Int
december25Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december25Solution2 :: IO Int
december25Solution2 = solution2 <$> input
