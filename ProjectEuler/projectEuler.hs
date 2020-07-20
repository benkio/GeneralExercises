module ProjectEuler where

import Data.List (find)
import Data.Maybe (fromJust)

-- Es 1

ex1_3_5_mult_sum :: Int
ex1_3_5_mult_sum = sum [x | x <- [1..1000], x `mod` 5 == 0 || x `mod` 3 == 0]

-- Es 2
fibonacciEvenSum :: Int
fibonacciEvenSum = sum $ takeWhile (< 4000000) $ filter even fibonacci
  where
    fibonacci = [1, 2] ++ zipWith (+) fibonacci (tail fibonacci)

-- Es 3

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = all (\p' -> n `mod` p' /= 0) [p | p <- [2..((round . sqrt . fromIntegral) n)], isPrime p]

primeFactors :: Int -> [Int]
primeFactors n
 | isPrime n = [1, n]
 | otherwise =
     let factor = fromJust $ find (\p -> isPrime p && n `mod` p == 0) [2..((round . sqrt . fromIntegral) n)]
     in factor : primeFactors (n `div` factor)

largestPrimeFactor :: Int
largestPrimeFactor = let target = 600851475143
                     in last $ primeFactors target
