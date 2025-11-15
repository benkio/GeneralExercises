module TwentyTwentyFour.December23 where

import Control.Arrow (second)
import Data.List (intercalate, isPrefixOf, maximumBy)
import Data.Map (Map, empty, keys, (!?))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Lib.Graph (fromConnectionList, longestConnectedPath, nextPaths, uniqueConnectedPathOfLength, uniquePathOfLength)

input :: IO [(String, String)]
input = parseInput <$> readFile "input/2024/December23.txt"

parseInput :: String -> [(String, String)]
parseInput = fmap (second tail . break (== '-')) . lines

testInput :: [(String, String)]
testInput =
    parseInput
        "kh-tc\n\
        \qp-kh\n\
        \de-cg\n\
        \ka-co\n\
        \yn-aq\n\
        \qp-ub\n\
        \cg-tb\n\
        \vc-aq\n\
        \tb-ka\n\
        \wh-tc\n\
        \yn-cg\n\
        \kh-ub\n\
        \ta-co\n\
        \de-co\n\
        \tc-td\n\
        \tb-wq\n\
        \wh-td\n\
        \ta-ka\n\
        \td-qp\n\
        \aq-cg\n\
        \wq-ub\n\
        \ub-vc\n\
        \de-ta\n\
        \wq-aq\n\
        \wq-vc\n\
        \wh-yn\n\
        \ka-de\n\
        \kh-ta\n\
        \co-tc\n\
        \wh-qp\n\
        \tb-vc\n\
        \td-yn\n"

solution1 :: [(String, String)] -> Int
solution1 x = length $ uniqueConnectedPathOfLength 3 i m
  where
    m = fromConnectionList x
    i = (: []) <$> (filter ("t" `isPrefixOf`) . keys) m

december23Solution1 :: IO Int
december23Solution1 = solution1 <$> input

test s i = longestConnectedPath [s] m empty
  where
    m = fromConnectionList testInput

test' = test "co" testInput

test'' s = test s <$> input

solution2 :: [(String, String)] -> String
solution2 x = intercalate "," . maximumBy (comparing length) $ mapMaybe (longest !?) ks
  where
    m = fromConnectionList x
    ks = (: []) <$> keys m
    longest = foldl (\r k -> longestConnectedPath k m r) empty ks

december23Solution2 :: IO String
december23Solution2 = solution2 <$> input
