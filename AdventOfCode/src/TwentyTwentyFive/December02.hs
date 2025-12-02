module TwentyTwentyFive.December02 where

import Data.List.Split (chunksOf, splitOn)
import Lib.List (pairs)
import Lib.Math (findDivisors)

input :: IO [(Int, Int)]
input = parseInput <$> readFile "input/2025/December02.txt"

parseInput :: String -> [(Int, Int)]
parseInput = fmap ((\[a, b] -> (read a :: Int, read b :: Int)) . splitOn "-") . splitOn ","

testInput :: [(Int, Int)]
testInput =
    parseInput
        "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

isInvalid :: Int -> Bool
isInvalid = uncurry (==) . (\x -> splitAt (length x `div` 2) x) . show

findInvalidIds :: (Int -> Bool) -> (Int, Int) -> [Int]
findInvalidIds f (a, b) = filter f [a .. b]

solution :: (Int -> Bool) -> [(Int, Int)] -> Int
solution f = sum . concatMap (findInvalidIds f)

december02Solution1 :: IO Int
december02Solution1 = solution isInvalid <$> input

isInvalid' :: Int -> Bool
isInvalid' x = any (\l -> foldr (\(t1, t2) b -> b && t1 == t2) ((not . null) l) l) splits
  where
    y = show x
    divisors = findDivisors (length y)
    splits = fmap (\n -> (pairs . chunksOf n) y) divisors

-- 66500947388 too high
december02Solution2 :: IO Int
december02Solution2 = solution isInvalid' <$> input
