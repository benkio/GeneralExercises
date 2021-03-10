module TwentyFifteen.TwentiethDecember where

import Data.List
import Data.Maybe

input :: IO Int
input = (\x -> read x :: Int) <$> readFile "input/2015/20December.txt"

solution1 :: Int -> Int
solution1 target =
  (fromJust . find (\x -> housePresents x >= target)) [1 .. target]

housePresents :: Int -> Int
housePresents = sum . fmap (* 10) . findDivisors

findDivisors :: Int -> [Int]
findDivisors x =
  ( concatMap
      ( \y ->
          if x `div` y == y
            then [y]
            else [y, x `div` y]
      )
      . filter (\i -> x `mod` i == 0)
  )
    [1 .. (ceiling (sqrt (fromIntegral x)))]

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

housePresents' :: Int -> Int
housePresents' i =
  (sum . fmap (* 11) . filter (\x -> x * 50 >= i) . findDivisors) i

solution2 :: Int -> Int
solution2 target =
  (fromJust . find (\x -> housePresents' x >= target)) [1 .. target]

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution2 <$> input
