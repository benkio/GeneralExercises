module TwentyTwentyFour.December09 where

input :: IO a
input = parseInput <$> readFile "input/2024/December09.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december09Solution1 :: IO Int
december09Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december09Solution2 :: IO Int
december09Solution2 = solution2 <$> input

