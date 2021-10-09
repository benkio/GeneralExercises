{-# LANGUAGE OverloadedStrings #-}

module ProjectEuler3 where

import Data.List (find, sort, (\\))
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as S (Set, fromList, toList, cartesianProduct, map, filter, notMember)
import qualified Data.Text as T (pack, replace, splitOn, unpack)
import ProjectEuler2 (findDivisors)

-- Es 21 ----------------------------------------------------------------------

amicableNum :: Int -> Maybe [Int]
amicableNum x =
  let divisorSum = (sum . filter (/= x) . S.toList . findDivisors) x
      amicableDivisorSum = (sum . filter (/= divisorSum) .  S.toList . findDivisors) divisorSum
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
isAbundant x = ((> x) . sum . filter (/= x). S.toList . findDivisors) x

abundantNumbers :: [Int]
abundantNumbers = [x | x <- [12..limitComposedAbundantNumbers], isAbundant x]

composedAbundantNumbers :: S.Set Int
composedAbundantNumbers = (S.filter (< limitComposedAbundantNumbers) . S.map (uncurry (+))) $ S.cartesianProduct (S.fromList abundantNumbers) (S.fromList abundantNumbers)

es23 :: Int
es23 = undefined -- (sum . filter (`S.notMember` composedAbundantNumbers)) [1..limitComposedAbundantNumbers]
