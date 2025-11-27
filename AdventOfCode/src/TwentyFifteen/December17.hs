module TwentyFifteen.December17 where

import Data.List

input :: IO [Int]
input = fmap read . lines <$> readFile "input/2015/17December.txt"

inputTest :: [Int]
inputTest = [20, 15, 5, 5, 10]

solution1 :: [Int] -> Int -> Int
solution1 xs target = (length . filter (target ==) . fmap sum) $ subsequences xs

solution1Test :: Bool
solution1Test = solution1 inputTest 25 == 4

solution2 :: [Int] -> Int -> Int
solution2 xs target =
    ( length
        . head
        . groupBy (\x y -> length x == length y)
        . sortOn length
        . filter ((target ==) . sum)
    )
        $ subsequences xs

solution2Test :: Bool
solution2Test = solution2 inputTest 25 == 3

december17Solution1 :: IO Int
december17Solution1 = (`solution1` 150) <$> input

december17Solution2 :: IO Int
december17Solution2 = (`solution2` 150) <$> input
