module TwentyTwentyFour.December23 where

import Control.Arrow (second)
import Data.List (isPrefixOf)
import Data.Map (keys)
import Lib.Graph (fromConnectionList, nextPaths, uniqueConnectedPathOfLength, uniquePathOfLength)

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
solution1 x = length . filter containsTPC $ uniqueConnectedPathOfLength 3 i m
  where
    m = fromConnectionList x
    i = (: []) <$> keys m
    containsTPC xs = any ("t" `isPrefixOf`) xs

december23Solution1 :: IO Int
december23Solution1 = solution1 <$> input

solution2 :: [(String, String)] -> Int
solution2 = undefined

december23Solution2 :: IO Int
december23Solution2 = solution2 <$> input
