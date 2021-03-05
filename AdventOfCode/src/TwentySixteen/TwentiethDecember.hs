module TwentySixteen.TwentiethDecember where

import           Data.List
import           Data.Maybe
import           Data.Set   (Set, empty, fromList, notMember, toList)
import qualified Data.Set   as Set (union)

input :: IO [(Int, Int)]
input =
  sortBy (\(a, _) (a', _) -> compare a a')
    . fmap parseRange
    . lines
    <$> readFile "input/2016/20December.txt"

parseRange :: String -> (Int, Int)
parseRange =
  (\(a, b) -> (read a :: Int, ((\x -> read x :: Int) . tail) b))
    . break ('-' ==)

invalidIPs :: [(Int, Int)] -> Set Int
invalidIPs = foldl (\s (a, b) -> Set.union s (fromList [a .. b])) empty

solution1Inefficient :: [(Int, Int)] -> Int
solution1Inefficient xs = (fromJust . find (\x -> x `notMember` invalidIPs xs)) [0 .. 4294967295]

solution1Test :: Bool
solution1Test =
  ( (== 3)
      . solution1 0
      . sortBy (\(a, _) (a', _) -> compare a a')
      . fmap parseRange
      . lines
  )
    "5-8\n\
    \0-2\n\
    \4-7"

solution1 :: Int -> [(Int, Int)] -> Int
solution1 v ((a, b) : xs)
  | v < a = v
  | v >= a && v <= b = solution1 (b + 1) xs
  | v >= a && v > b = solution1 v xs

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 0 <$> input

solution2Inefficient :: [(Int, Int)] -> Int
solution2Inefficient = length . ([0 .. 4294967295] \\) . toList . invalidIPs

solution2 :: Int -> Int -> Int -> [(Int, Int)] -> Int
solution2 v acc up [] = acc + (up - v)
solution2 v acc up ((a, b) : xs)
  | v < a = solution2 (b + 1) (acc + (a - v)) up xs
  | v >= a && v <= b = solution2 (b + 1) acc up xs
  | v >= a && v > b = solution2 v acc up xs

solution2Test :: Bool
solution2Test =
  ( (== 2)
      . solution2 0 0 10
      . sortBy (\(a, _) (a', _) -> compare a a')
      . fmap parseRange
      . lines
  )
    "5-8\n\
    \0-2\n\
    \4-7"

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution2 0 0 4294967296 <$> input
