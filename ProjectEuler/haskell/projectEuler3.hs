{-# LANGUAGE OverloadedStrings #-}

module ProjectEuler3 where

import Data.List (find, sort, nub)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T (pack, replace, splitOn, unpack)
import ProjectEuler2 (findDivisors)

-- Es 21 ----------------------------------------------------------------------

amicableNum :: Int -> Maybe [Int]
amicableNum x =
  let divisorSum = (sum . filter (/= x) . findDivisors) x
      amicableDivisorSum = (sum . filter (/= divisorSum) . findDivisors) divisorSum
   in if amicableDivisorSum == x && x /= divisorSum then Just [x, divisorSum] else Nothing

findAmicables :: [Int] -> [Int] -> [Int]
findAmicables [] amicables = amicables
findAmicables (x : xs) amicables
  | x `elem` amicables = findAmicables xs amicables
  | isJust maybeAmicables = findAmicables xs (amicables ++ fromJust maybeAmicables)
  | otherwise = findAmicables xs amicables
  where
    maybeAmicables = amicableNum x

es21 :: Int
es21 = sum $ findAmicables [1 .. 10000] []

-- Es 22 ---------------------------------------------------------------

es22Input :: IO [String]
es22Input = sort . fmap (T.unpack . T.replace "\"" "") . T.splitOn "\",\"" . T.pack <$> readFile "p022_names.txt"

alphabetIndexed :: [(Char, Int)]
alphabetIndexed = ['A' .. 'Z'] `zip` [1 ..]

nameScore :: String -> Int
nameScore = sum . fmap (\c -> maybe 0 snd $ find ((== c) . fst) alphabetIndexed)

es22 :: IO Int
es22 = sum . zipWith (*) [1 ..] . fmap nameScore <$> es22Input

-- Es 23 ---------------------------------------------------------------

limitComposedAbundantNumbers :: Int
limitComposedAbundantNumbers = 28123

isAbundant :: Int -> Bool
isAbundant x = ((> x) . sum . filter (/= x). findDivisors) x

abundantNumbers :: [Int]
abundantNumbers = [x | x <- [12..limitComposedAbundantNumbers], isAbundant x]

isNotAbundantComposable :: Int -> Bool
isNotAbundantComposable x = go x $ filter (<= x) abundantNumbers
  where go :: Int -> [Int] -> Bool
        go _ [] = True
        go a (b:bs)
          | (a - b) `elem` (b:bs) = False
          | otherwise = go a bs
  
es23 :: Int
es23 = go [1..7000] 0 --(sum . filter isNotAbundantComposable) [1..limitComposedAbundantNumbers]
  where go [] acc = acc
        go (x:xs) acc
          | isNotAbundantComposable x = go xs (acc + x)
          | otherwise = go xs acc
