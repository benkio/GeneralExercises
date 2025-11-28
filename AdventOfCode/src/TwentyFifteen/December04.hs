module TwentyFifteen.December04 where

import qualified Data.ByteString.Lazy.Char8 as B
import Lib.MD5 (generateMD5)

input :: IO String
input = readFile "input/2015/4December.txt"

inputTest :: String
inputTest =
    "abcdef\n\
    \pqrstuv"

testSolution1 :: Bool
testSolution1 =
    ((\l -> l == [609043, 1048970]) . fmap solution1 . lines) inputTest

bruteSearchLeadingZeros :: String -> [Int] -> Int -> Int
bruteSearchLeadingZeros prefix (x : xs) zeros
    | all ('0' ==) md5Prefix = x
    | otherwise = bruteSearchLeadingZeros prefix xs zeros
  where
    md5Prefix = take zeros (generateMD5 (prefix ++ show x))

solution1 :: String -> Int
solution1 s = bruteSearchLeadingZeros s [0 ..] 5

solution2 :: String -> Int
solution2 s = bruteSearchLeadingZeros s [0 ..] 6

december04Solution1 :: IO Int
december04Solution1 = solution1 . init <$> input

december04Solution2 :: IO Int
december04Solution2 = solution2 . init <$> input
