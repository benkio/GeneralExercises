module TwentyTwentyFour.December11 where

input :: IO a
input = parseInput <$> readFile "input/2024/December11.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december11Solution1 :: IO Int
december11Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december11Solution2 :: IO Int
december11Solution2 = solution2 <$> input

