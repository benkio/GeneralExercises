module TwentyFifteen.FourthDecember where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Digest.Pure.MD5       as M

input :: IO String
input = readFile "input/2015/4December.txt"

inputTest :: String
inputTest =
  "abcdef\n\
            \pqrstuv"

testSolution1 :: Bool
testSolution1 =
  ((\l -> l == [609043, 1048970]) . fmap solution1 . lines) inputTest

generateMD5 :: String -> Int -> String
generateMD5 prefix num =
  let md5input = B.pack $ prefix ++ show num
   in show $ M.md5 md5input

bruteSearchLeadingZeros :: String -> [Int] -> Int -> Int
bruteSearchLeadingZeros prefix (x:xs) zeros
  | all ('0' ==) md5Prefix = x
  | otherwise = bruteSearchLeadingZeros prefix xs zeros
  where
    md5Prefix = take zeros (generateMD5 prefix x)

solution1 :: String -> Int
solution1 s = bruteSearchLeadingZeros s [0 ..] 5

solution2 :: String -> Int
solution2 s = bruteSearchLeadingZeros s [0 ..] 6

fourthDecemberSolution1 :: IO Int
fourthDecemberSolution1 = solution1 . init <$> input

fourthDecemberSolution2 :: IO Int
fourthDecemberSolution2 = solution2 . init <$> input
