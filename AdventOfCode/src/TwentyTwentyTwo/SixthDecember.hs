module TwentyTwentyTwo.SixthDecember where

import Data.List (find, group, sort)

input :: IO String
input = readFile "input/2022/6December.txt"

testInput :: [String]
testInput =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

groupBy :: Int -> [a] -> [[a]]
groupBy i s = take i s : groupBy i (tail s)

solution :: Int -> String -> Int
solution i = maybe (error "impossible") (fst . last) . find allDifferent . groupBy i . ([1 ..] `zip`)
  where
    allDifferent :: [(Int, Char)] -> Bool
    allDifferent = all ((== 1) . length) . group . sort . fmap snd

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = solution 4 <$> input

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = solution 14 <$> input
