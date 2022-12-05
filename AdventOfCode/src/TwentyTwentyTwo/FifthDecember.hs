module TwentyTwentyTwo.FifthDecember where

import Data.Map (Map, adjust, fromList, toList, (!))
import Debug.Trace
import Text.Printf

type Stack = String

newtype Cargo = Cargo (Map Int Stack) deriving (Show)

data Move = Move
  { amount :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

input :: IO [Move]
input = fmap parseInput . tail . dropWhile (/= "") . lines <$> readFile "input/2022/5December.txt"

--     [D]
-- [N] [C]
-- [Z] [M] [P]
--  1   2   3

testInitialCargo :: Cargo
testInitialCargo = Cargo $ fromList [(1, "NZ"), (2, "DCM"), (3, "P")]

testInput :: [String]
testInput =
  lines
    "move 1 from 2 to 1\n\
    \move 3 from 1 to 3\n\
    \move 2 from 2 to 1\n\
    \move 1 from 1 to 2"

parseInput :: String -> Move
parseInput s =
  let extractor = (\x -> (read (takeWhile (/= ' ') x) :: Int, (drop 2 . dropWhile (/= ' ')) x)) . tail . dropWhile (/= ' ')
      (amount, restAmount) = extractor s
      (from, restFrom) = extractor restAmount
      (to, _) = extractor restFrom
   in Move {amount = amount, from = from, to = to}

applyMove9000 :: Cargo -> Move -> Cargo
applyMove9000 (Cargo c) m@Move {amount = a, from = f, to = t}
  | a == 0 = Cargo c
  | otherwise =
    let (crate, from') =
          (\xs -> (head xs, tail xs)) $ c ! f
        to' = (crate :) $ c ! t
        c' = adjust (const to') t $ adjust (const from') f c
     in applyMove9000 (Cargo c') $ m {amount = a - 1}

solution1 :: Cargo -> [Move] -> String
solution1 c = extractTopStack . foldl applyMove9000 c
  where
    extractTopStack :: Cargo -> String
    extractTopStack (Cargo m) = (\(_, s) -> head s) <$> toList m

-- [M] [S] [S]
--         [M] [N] [L] [T] [Q]
-- [G]     [P] [C] [F] [G] [T]
-- [B]     [J] [D] [P] [V] [F] [F]
-- [D]     [D] [G] [C] [Z] [H] [B] [G]
-- [C] [G] [Q] [L] [N] [D] [M] [D] [Q]
-- [P] [V] [S] [S] [B] [B] [Z] [M] [C]
-- [R] [H] [N] [P] [J] [Q] [B] [C] [F]
--  1   2   3   4   5   6   7   8   9

initialCargo :: Cargo
initialCargo =
  Cargo $
    fromList
      [ (1, "GBDCPR"),
        (2, "GVH"),
        (3, "MPJDQSN"),
        (4, "MNCDGLSP"),
        (5, "SLFPCNBJ"),
        (6, "STGVZDBQ"),
        (7, "QTFHMZB"),
        (8, "FBDMC"),
        (9, "GQCF")
      ]

applyMove9001 :: Cargo -> Move -> Cargo
applyMove9001 (Cargo c) m@Move {amount = a, from = f, to = t}
  | a == 0 = Cargo c
  | otherwise =
    let (crate, from') =
          splitAt a $ c ! f
        to' = (crate ++) $ c ! t
        c' = adjust (const to') t $ adjust (const from') f c
     in Cargo c'

solution2 :: Cargo -> [Move] -> String
solution2 c = extractTopStack . foldl applyMove9001 c
  where
    extractTopStack :: Cargo -> String
    extractTopStack (Cargo m) = (\(_, s) -> head s) <$> toList m

fifthDecemberSolution1 :: IO String
fifthDecemberSolution1 = solution1 initialCargo <$> input

fifthDecemberSolution2 :: IO String
fifthDecemberSolution2 = solution2 initialCargo <$> input
