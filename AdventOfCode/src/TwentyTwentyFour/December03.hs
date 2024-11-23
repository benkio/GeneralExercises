module TwentyTwentyFour.December03 where

input :: IO a
input = parseInput <$> readFile "input/2024/December03.txt"

parseInput :: String -> a
parseInput = undefined

testInput :: a
testInput =
    parseInput
        ""

solution1 :: a -> Int
solution1 = undefined

december03Solution1 :: IO Int
december03Solution1 = solution1 <$> input

solution2 :: a -> Int
solution2 = undefined

december03Solution2 :: IO Int
december03Solution2 = solution2 <$> input

