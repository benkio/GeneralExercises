module TwentyTwentyFive.December20 where

input :: IO a
input = parseInput <$> readFile "input/2025/December20.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december20Solution1 :: IO Int
december20Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december20Solution2 :: IO Int
december20Solution2 = solution2 <$> input
