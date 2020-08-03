module ProjectEuler where

import Control.Parallel.Strategies (using, rpar, parListChunk)
import Data.List (find, sort)
import Data.Maybe (fromJust, isJust)

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
    primes' n ps = (n : ps'') ++  primes' (head ps''') (tail ps''')
      where ps' = filter (\x -> x `mod` n /= 0) ps
            ps'' = takeWhile (< n*n) ps'
            ps''' = [x | x <- filter (>= n*n) ps', all (\y -> x `mod` y /= 0) ps'']

oneThousandOnePrime :: Int
oneThousandOnePrime = primes !! 10001

-- Es 8
chunksOfMax :: Int -> Int -> String -> Integer
chunksOfMax acc 0 _ = toInteger acc
chunksOfMax acc n [] = toInteger acc
chunksOfMax acc n xs =
  let current = foldl (\x y -> x * (read [y] :: Int)) 1 (take n xs)
  in if current > acc then chunksOfMax current n (tail xs) else chunksOfMax acc n (tail xs)

largestProductInSeries :: Integer
largestProductInSeries =
  let inputNum :: Integer
      inputNum = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
  in chunksOfMax 0 13 (show inputNum)

-- Es 9
findMN :: (Int, Int)
findMN = findCoeff [1..]
  where
    formula m =
      let (d, r) = (500 `divMod` m)
      in if r == 0 then Just (d - m) else Nothing
    findCoeff (n:ns)
      | isJust m && (fromJust m) ^ 2 < n ^ 2 = (n, fromJust m)
      | otherwise = findCoeff ns
        where m = formula n

pythagorianTriplet1000Product :: Int
pythagorianTriplet1000Product =
  let (m, n) = findMN
      a = m^2 - n^2
      b = 2 * m * n
      c = m^2 + n^2
  in a * b * c

-- Es 10
sumTwoMilionPrimes :: Int
sumTwoMilionPrimes = sumInChunk primes 200000
  where sumInChunk :: [Int] -> Int -> Int
        sumInChunk ps limit
          | limit < 1000 = sum $ take limit ps
          | otherwise = let (chunk, rest) =  splitAt 1000 ps
                        in sum chunk + sumInChunk rest (limit - 1000)
