module TwentyTwentyFour.December09 where

import Data.Bifunctor (bimap, first, second)
import Data.List (group)
import Data.Maybe (fromJust, isJust)

-- import Text.Printf
import Text.Read (readMaybe)

-- import Debug.Trace

input :: IO [Int]
input = parseInput . init <$> readFile "input/2024/December09.txt"

parseInput :: String -> [Int]
parseInput = fmap (\x -> read [x] :: Int)

generateFileSystem :: [Int] -> [Int]
generateFileSystem = concat . zipWith generateFilesOrSpace [0 ..]

generateFilesOrSpace :: Int -> Int -> [Int]
generateFilesOrSpace i v
    | even i = replicate v (i `div` 2) -- file
    | otherwise = replicate v (-1) -- free space

defrag :: [Int] -> [Int]
defrag [] = []
defrag xs = merge content rest
  where
    totalSpace = length . filter (== (-1)) $ xs
    (content, rest) = second (reverse . filter (/= (-1))) . splitAt (length xs - totalSpace) $ xs

merge :: [Int] -> [Int] -> [Int]
merge [] xs = []
merge xs ys = content ++ toInsert ++ merge restXs restYs
  where
    (content, (space, restXs)) = second (span (== (-1))) $ span (/= (-1)) xs
    (toInsert, restYs) = splitAt (length space) ys

testInput :: [Int]
testInput = parseInput "2333133121414131402"

checksum :: [Int] -> Int
checksum = foldl (\acc (i, c) -> if c < 0 then acc else acc + i * c) 0 . zip [0 ..]

solution1 :: [Int] -> Int
solution1 =
    checksum
        . defrag
        . generateFileSystem

-- too low 90167081070
december09Solution1 :: IO Int
december09Solution1 = solution1 <$> input

insertInFirstAvailableSpace :: [Int] -> [[Int]] -> [[Int]]
insertInFirstAvailableSpace vs xss =
    let (initial, space : rest) = span (\xs -> any (/= (-1)) xs || length xs < length vs) xss
     in initial ++ [vs, replicate (length space - length vs) (-1)] ++ rest

extractValue :: [Int] -> [[Int]] -> [[Int]]
extractValue vs xss =
    let (initial, _ : yss) = span (/= vs) xss
     in initial ++ replicate (length vs) (-1) : yss

defragSingle :: [Int] -> [[Int]] -> [[Int]]
defragSingle vs = insertInFirstAvailableSpace vs . extractValue vs

defrag2 :: [Int] -> [Int]
defrag2 xs = concat $ foldr defragSingle xss files
  where
    xss = group xs
    files = filter (any (/= (-1))) xss

solution2 :: [Int] -> Int
solution2 = checksum . defrag2 . generateFileSystem

december09Solution2 :: IO Int
december09Solution2 = solution2 <$> input
