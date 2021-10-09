{-# LANGUAGE OverloadedStrings #-}

module ProjectEuler3 where


import Data.List (find, sort, (\\), tails)
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set (Set, fromList, toList, cartesianProduct, map, filter, notMember, empty, union)
import qualified Data.Text as T (pack, replace, splitOn, unpack)
import ProjectEuler2 (findDivisors)
import ProjectEuler

-- Es 21 ----------------------------------------------------------------------

amicableNum :: Int -> Maybe [Int]
amicableNum x =
  let divisorSum = (sum . findDivisors) x
      amicableDivisorSum = (sum . findDivisors) divisorSum
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
es22Input = sort . fmap (T.unpack . T.replace "\"" "") . T.splitOn "\",\"" . T.pack <$> readFile "data/p022_names.txt"

alphabetIndexed :: [(Char, Int)]
alphabetIndexed = ['A' .. 'Z'] `zip` [1 ..]

nameScore :: String -> Int
nameScore = sum . fmap (\c -> maybe 0 snd $ find ((== c) . fst) alphabetIndexed)

es22 :: IO Int
es22 = sum . zipWith (*) [1 ..] . fmap nameScore <$> es22Input

-- Es 23 ---------------------------------------------------------------

limit :: Int
limit = 28123

isAbundant :: Int -> Bool
isAbundant x = ((> x) . sum . findDivisors) x

abundants :: [Int]
abundants = [x | x <- [1..limit], isAbundant x]

abundantsSum :: Set.Set Int
abundantsSum = Set.fromList [ x + y | (x:ys) <- tails abundants, y <- x:ys, x + y <= 28123 ]

es23 :: Int
es23 = sum $ [1..limit] \\ Set.toList abundantsSum
