module TwentyTwentyFive.December14 where

input :: IO a
input = parseInput <$> readFile "input/2025/December14.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december14Solution1 :: IO Int
december14Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december14Solution2 :: IO Int
december14Solution2 = solution2 <$> input
