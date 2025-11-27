module TwentyTwentyTwo.December06 where

import Data.List (find, group, sort)

input :: IO String
input = readFile "input/2022/6December.txt"

testInput :: [String]
testInput =
    [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    , "bvwbjplbgvbhsrlpgdmjqwftvncz"
    , "nppdvjthqldpwncqszvftbrmjlhg"
    , "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    , "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
    ]

groupBy :: Int -> [a] -> [[a]]
groupBy i s = take i s : groupBy i (tail s)

solution :: Int -> String -> Int
solution i = maybe (error "impossible") (fst . last) . find allDifferent . groupBy i . ([1 ..] `zip`)
  where
    allDifferent :: [(Int, Char)] -> Bool
    allDifferent = all ((== 1) . length) . group . sort . fmap snd

december06Solution1 :: IO Int
december06Solution1 = solution 4 <$> input

december06Solution2 :: IO Int
december06Solution2 = solution 14 <$> input
