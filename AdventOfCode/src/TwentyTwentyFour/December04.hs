module TwentyTwentyFour.December04 where

input :: IO a
input = parseInput <$> readFile "input/2024/December04.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december04Solution1 :: IO Int
december04Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december04Solution2 :: IO Int
december04Solution2 = solution2 <$> input

