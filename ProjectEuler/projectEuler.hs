module ProjectEuler where

import Data.List (find, sort)
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

-- Es 4
isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse . show) n

largest3DigitPalindrome :: Int
largest3DigitPalindrome =  (maximum . filter isPalindrome) [x * y | x <- [100..999], y <- [100..999]]

-- Es 5
smallestMultiple :: Int
smallestMultiple = head [x | x <- [20, 40..], all (\p -> x `mod` p == 0 ) [11..20]]

-- Es 6
sumSquareDifference :: Int
sumSquareDifference =
  let squared100N = foldl (+) 0 [x ^ 2 | x <- [1..100]]
      sumSquared100N = (^2) $ foldl (+) 0 [1..100]
  in sumSquared100N - squared100N

-- Es 7
primes :: [Int]
primes =  1 : primes' 2 [3..]
  where
    primes' :: Int -> [Int] -> [Int]
    primes' n ps
      | isPrime n = n : primes' (head ps') (tail ps')
      | otherwise = primes' (head ps) (tail ps)
      where ps' = filter (\x -> x `mod` n /= 0) ps

oneThousendOnePrime :: Int
oneThousendOnePrime = primes !! 10001
