module ProjectEuler3 where

import ProjectEuler2 (findDivisors)
import Data.Maybe (isJust, fromJust)

-- Es 21 ----------------------------------------------------------------------

amicableNum :: Int -> Maybe [Int]
amicableNum x =
  let divisorSum = (sum . filter (/= x) . findDivisors) x
      amicableDivisorSum = (sum . filter (/= divisorSum) . findDivisors) divisorSum
  in if amicableDivisorSum == x && x /= divisorSum then Just [x, divisorSum] else Nothing

findAmicables :: [Int] -> [Int] -> [Int]
findAmicables [] amicables = amicables
findAmicables (x:xs) amicables
  | x `elem` amicables = findAmicables xs amicables
  | isJust maybeAmicables = findAmicables xs (amicables ++ fromJust maybeAmicables)
  | otherwise = findAmicables xs amicables
    where maybeAmicables = amicableNum x

es21 :: Int
es21 = sum $ findAmicables [1..10000] []
