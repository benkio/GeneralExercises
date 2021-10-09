module ProjectEuler where

import Control.Parallel.Strategies (using, rpar, parListChunk)
import Data.List (find, sort)
import Data.Maybe (fromJust, isJust)

-- Es 1
es1 :: Int
es1 = sum [x | x <- [1..1000], x `mod` 5 == 0 || x `mod` 3 == 0]

-- Es 2
es2 :: Int
es2 = sum $ takeWhile (< 4000000) $ filter even fibonacci
  where
    fibonacci = [1, 2] ++ zipWith (+) fibonacci (tail fibonacci)

-- Es 3
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps

es3 :: Int
es3 = let target = 600851475143
                     in last $ primeFactors target

-- Es 4
isPalindrome :: Int -> Bool
isPalindrome n = show n == (reverse . show) n

es4 :: Int
es4 =  (maximum . filter isPalindrome) [x * y | x <- [100..999], y <- [100..999]]

-- Es 5
es5 :: Int
es5 = head [x | x <- [20, 40..], all (\p -> x `mod` p == 0 ) [11..20]]

-- Es 6
es6 :: Int
es6 =
  let squared100N = foldl (+) 0 [x ^ 2 | x <- [1..100]]
      sumSquared100N = (^2) $ foldl (+) 0 [1..100]
  in sumSquared100N - squared100N

-- Es 7
es7 :: Int
es7 = primes !! 10001

-- Es 8
chunksOfMax :: Int -> Int -> String -> Integer
chunksOfMax acc 0 _ = toInteger acc
chunksOfMax acc n [] = toInteger acc
chunksOfMax acc n xs =
  let current = foldl (\x y -> x * (read [y] :: Int)) 1 (take n xs)
  in if current > acc then chunksOfMax current n (tail xs) else chunksOfMax acc n (tail xs)

es8 :: Integer
es8 =
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

es9 :: Int
es9 =
  let (m, n) = findMN
      a = m^2 - n^2
      b = 2 * m * n
      c = m^2 + n^2
  in a * b * c

-- Es 10
es10 :: Int
es10 = (sum . takeWhile (<2000000)) primes
