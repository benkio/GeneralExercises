module TwentyTwentyFive.December06 where

input :: IO a
input = parseInput <$> readFile "input/2025/December06.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december06Solution1 :: IO Int
december06Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
