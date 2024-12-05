{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December05 where

import Data.Bifunctor (bimap)
import Data.IntMap (IntMap, fromListWith, (!?))
import Data.List (find)
import Data.List.Split (wordsBy)
import Data.Maybe (isJust, isNothing, mapMaybe)

type Rules = IntMap [Int]
type Updates = [[Int]]

input :: IO (Rules, Updates)
input = parseInput <$> readFile "input/2024/December05.txt"

parseInput :: String -> (Rules, Updates)
parseInput = bimap (fromListWith (++) . fmap parseRule) (fmap parseUpdate . tail) . break (== "") . lines
  where
    parseRule :: String -> (Int, [Int])
    parseRule s =
        let
            start = read (take 2 s) :: Int
            end = ((\x -> read x :: Int) . take 2 . drop 3) s
         in
            (start, [end])
    parseUpdate :: String -> [Int]
    parseUpdate = fmap ((\x -> read x :: Int) . take 2) . wordsBy (== ',')

isValid :: Rules -> (Int, Int) -> Maybe (Int, Int)
isValid m (v, k) =
    (m !? k) >>= find (== v) >> return (v, k)

precendencePairs :: [Int] -> [(Int, Int)]
precendencePairs [] = []
precendencePairs (x : xs) = fmap (x,) xs ++ precendencePairs xs

midValue :: [Int] -> Int
midValue ls = ((ls !!) . (`div` 2) . length) ls

solution1 :: (Rules, Updates) -> Int
solution1 (rules, updates) = sum $ midValue <$> filter (all (isNothing . isValid rules) . precendencePairs) updates

december05Solution1 :: IO Int
december05Solution1 = solution1 <$> input

swap :: (Int, Int) -> [Int] -> [Int]
swap _ [] = []
swap (a, b) (x : xs)
    | x == a = b : swap (a, b) xs
    | x == b = a : swap (a, b) xs
    | otherwise = x : swap (a, b) xs

fixUpdate :: Rules -> [Int] -> [Int]
fixUpdate m u = if null (invalidPairs fixAttempt) then fixAttempt else fixUpdate m fixAttempt
  where
    invalidPairs x = mapMaybe (isValid m) $ precendencePairs x
    fixAttempt = foldl (\acc (a, b) -> swap (a, b) acc) u (invalidPairs u)

solution2 :: (Rules, Updates) -> Int
solution2 (rules, updates) = sum $ midValue . fixUpdate rules <$> filter (any (isJust . isValid rules) . precendencePairs) updates

december05Solution2 :: IO Int
december05Solution2 = solution2 <$> input

testInput =
    parseInput
        "47|53\n\
        \97|13\n\
        \97|61\n\
        \97|47\n\
        \75|29\n\
        \61|13\n\
        \75|53\n\
        \29|13\n\
        \97|29\n\
        \53|29\n\
        \61|53\n\
        \97|53\n\
        \61|29\n\
        \47|13\n\
        \75|47\n\
        \97|75\n\
        \47|61\n\
        \75|61\n\
        \47|29\n\
        \75|13\n\
        \53|13\n\
        \\n\
        \75,47,61,53,29\n\
        \97,61,53,29,13\n\
        \75,29,13\n\
        \75,97,47,61,53\n\
        \61,13,29\n\
        \97,13,75,29,47"
