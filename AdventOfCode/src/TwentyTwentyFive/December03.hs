module TwentyTwentyFive.December03 where

import Lib.List (pairsWith)
import Lib.Math (intsToInt)

type BatteryBank = [Int]

input :: IO [BatteryBank]
input = parseInput <$> readFile "input/2025/December03.txt"

parseInput :: String -> [BatteryBank]
parseInput = fmap parseBatteryBank . lines
  where
    parseBatteryBank :: String -> BatteryBank
    parseBatteryBank = fmap (\x -> read [x] :: Int)

testInput :: [BatteryBank]
testInput =
    parseInput
        "987654321111111\n\
        \811111111111119\n\
        \234234234234278\n\
        \818181911112111\n"

joltsToInt :: Int -> Int -> Int
joltsToInt x y = x * 10 + y

findMaxJolts :: BatteryBank -> Int
findMaxJolts = maximum . pairsWith joltsToInt

solution :: (BatteryBank -> Int) -> [BatteryBank] -> Int
solution f = sum . fmap f

december03Solution1 :: IO Int
december03Solution1 = solution findMaxJolts <$> input

findMaxJolts' :: BatteryBank -> Int
findMaxJolts' xs = intsToInt $ go 12 xs
  where
    go n [] = []
    go n ys@(x : xs)
        | n == length ys = ys
        | n == 1 = [bestCandidate n ys]
        | n > length ys = error "Can't take more then there are"
        | otherwise =
            if x == bestCandidate n ys
                then x : go (n - 1) xs
                else go n xs
    bestCandidate n xs = maximum $ take (length xs - n + 1) xs

december03Solution2 :: IO Int
december03Solution2 = solution findMaxJolts' <$> input
