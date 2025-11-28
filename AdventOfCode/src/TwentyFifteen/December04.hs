module TwentyFifteen.December04 where

import Data.Maybe (fromMaybe)
import Lib.MD5 (generateMD5)
import Lib.Par (findPar)

input :: IO String
input = readFile "input/2015/4December.txt"

inputTest :: String
inputTest =
    "abcdef\n\
    \pqrstuv"

testSolution1 :: Bool
testSolution1 =
    ((\l -> l == [609043, 1048970]) . fmap solution1 . lines) inputTest

bruteSearchLeadingZeros :: String -> [Int] -> Int -> Maybe Int
bruteSearchLeadingZeros prefix xs zeros =
    findPar (all ('0' ==) . md5Prefix) xs
  where
    md5Prefix x = take zeros $ generateMD5 (prefix ++ show x)

solution1 :: String -> Int
solution1 s = fromMaybe (-1) $ bruteSearchLeadingZeros s [0 .. maxBoundSearch] 5

solution2 :: String -> Int
solution2 s = fromMaybe (-1) $ bruteSearchLeadingZeros s [0 .. maxBoundSearch] 6

maxBoundSearch :: Int
maxBoundSearch = 10000000

december04Solution1 :: IO Int
december04Solution1 = solution1 . init <$> input

december04Solution2 :: IO Int
december04Solution2 = solution2 . init <$> input
