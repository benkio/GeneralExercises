module TwentyTwentyFour.December01 where

input :: IO a
input = parseInput <$> readFile "input/2024/December01.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december01Solution1 :: IO Int
december01Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december01Solution2 :: IO Int
december01Solution2 = solution2 <$> input

