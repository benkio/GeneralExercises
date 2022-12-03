module TwentyTwentyTwo.FirstDecember where

import Data.Maybe (maybe)
import Text.Read (readMaybe)
import Data.List

input :: IO [String]
input = lines <$> readFile "input/2022/1December.txt"

testInput :: String
testInput =
  "1000\n\
  \2000\n\
  \3000\n\
  \\n\
  \4000\n\
  \\n\
  \5000\n\
  \6000\n\
  \\n\
  \7000\n\
  \8000\n\
  \9000\n\
  \\n\
  \10000"

inputParse :: [String] -> [[Int]]
inputParse = (\(acc, last) -> acc ++ [last]) . foldl (\(acc, lacc) sv -> (maybe (acc ++ [lacc], []) (\v -> (acc, lacc ++ [v])) . readMaybe) sv) ([], [])

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = maximum . fmap sum . inputParse <$> input

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = sum . take 3 . sortBy (flip compare) . fmap sum . inputParse <$> input
