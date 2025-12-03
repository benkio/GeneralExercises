module Lib.Math (
    twoLinearEqSolver,
    isInteger,
    calculateSlope,
    areParallel,
    findDivisors,
    intsToInt,
)
where

import Data.List (union)

{-
input (a,b) (c,d) (r1,r2)

ax + by = c
cx + dy = c2
-}
twoLinearEqSolver :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
twoLinearEqSolver a b c1 c d c2
    | determinant == 0 = Nothing
    | y * determinant /= a * c2 - c * c1 = Nothing
    | x * (a * determinant) /= c1 * determinant - b * (a * c2 - c * c1) = Nothing
    | otherwise = Just (x, y)
  where
    determinant = a * d - c * b
    yNumerator = a * c2 - c * c1
    xNumerator = c1 * determinant - b * yNumerator
    y = yNumerator `div` determinant
    x = xNumerator `div` (a * determinant)

{-
ax + by = z
-}
calculateSlope :: Int -> Int -> Int
calculateSlope a b = -(a `div` b)

areParallel :: (Int, Int) -> (Int, Int) -> Bool
areParallel (a, b) (c, d) = calculateSlope a b == calculateSlope c d

isInteger :: (RealFrac a) => a -> Bool
isInteger n = floor n == ceiling n

findDivisors :: Int -> [Int]
findDivisors n =
    let
        isqrt :: Int -> Int
        isqrt x = floor . sqrt $ (fromIntegral x :: Float)
        divisors = filter ((== 0) . mod n) [1 .. isqrt n]
        divisors' = (fmap (div n) . drop 1) divisors
     in
        divisors `union` divisors'

intsToInt :: [Int] -> Int
intsToInt = sum . zipWith (*) (fmap (10 ^) [0 ..]) . reverse
