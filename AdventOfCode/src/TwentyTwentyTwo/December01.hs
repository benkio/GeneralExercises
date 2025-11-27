module TwentyTwentyTwo.December01 where

import Data.List
import Data.Maybe (maybe)
import Text.Read (readMaybe)

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

december01Solution1 :: IO Int
december01Solution1 = maximum . fmap sum . inputParse <$> input

december01Solution2 :: IO Int
december01Solution2 = sum . take 3 . sortBy (flip compare) . fmap sum . inputParse <$> input
