module TwentyFifteen.December01 where

import Data.List (group, sort)

input :: IO String
input = readFile "input/2015/1December.txt"

inputTest :: String
inputTest = "(())"

solution1 :: String -> Int
solution1 = (\l -> head l - last l) . fmap length . group . sort

solution2 :: (Int, Int) -> String -> Int
solution2 (i, _) [] = i
solution2 (i, f) (x : xs)
    | f == -1 = i
    | x == '(' = solution2 (i + 1, f + 1) xs
    | x == ')' = solution2 (i + 1, f - 1) xs

december01Solution1 :: IO Int
december01Solution1 = solution1 . filter (\x -> x == '(' || x == ')') <$> input

december01Solution2 :: IO Int
december01Solution2 = solution2 (0, 0) <$> input
