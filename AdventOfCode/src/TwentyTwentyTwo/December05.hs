module TwentyTwentyTwo.December05 where

import Data.List (transpose)
import Data.Map (Map, adjust, fromList, toList, (!))

type Stack = String

newtype Cargo = Cargo (Map Int Stack) deriving (Show, Eq)

data Move = Move
    { amount :: Int
    , from :: Int
    , to :: Int
    }
    deriving (Show)

input :: IO (Cargo, [Move])
input = (\xs -> (parseCargo xs, (fmap parseMoves . tail . dropWhile (/= "")) xs)) . lines <$> readFile "input/2022/5December.txt"

testInitialCargo :: Cargo
testInitialCargo = parseCargo testInput

parseCargo :: [String] -> Cargo
parseCargo = Cargo . fromList . ([1 ..] `zip`) . fmap (foldl1 (<>)) . transpose . fmap parseLine . init . takeWhile (/= "")
  where
    parseLine :: String -> [String]
    parseLine "" = []
    parseLine s = (filter (\x -> not (x == '[' || x == ']' || x == ' ')) . take 3) s : parseLine (drop 4 s)

testInput :: [String]
testInput =
    lines
        "    [D]    \n\
        \[N] [C]    \n\
        \[Z] [M] [P]\n\
        \ 1   2   3 \n\
        \\n\
        \move 1 from 2 to 1\n\
        \move 3 from 1 to 3\n\
        \move 2 from 2 to 1\n\
        \move 1 from 1 to 2"

parseMoves :: String -> Move
parseMoves s =
    let extractor = (\x -> (read (takeWhile (/= ' ') x) :: Int, (drop 2 . dropWhile (/= ' ')) x)) . tail . dropWhile (/= ' ')
        (amount, restAmount) = extractor s
        (from, restFrom) = extractor restAmount
        (to, _) = extractor restFrom
     in Move{amount = amount, from = from, to = to}

applyMove9000 :: Cargo -> Move -> Cargo
applyMove9000 (Cargo c) m@Move{amount = a, from = f, to = t}
    | a == 0 = Cargo c
    | otherwise =
        let (crate, from') =
                (\xs -> (head xs, tail xs)) $ c ! f
            to' = (crate :) $ c ! t
            c' = adjust (const to') t $ adjust (const from') f c
         in applyMove9000 (Cargo c') $ m{amount = a - 1}

solution1 :: Cargo -> [Move] -> String
solution1 c = extractTopStack . foldl applyMove9000 c
  where
    extractTopStack :: Cargo -> String
    extractTopStack (Cargo m) = (\(_, s) -> head s) <$> toList m

applyMove9001 :: Cargo -> Move -> Cargo
applyMove9001 (Cargo c) m@Move{amount = a, from = f, to = t}
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

december05Solution1 :: IO String
december05Solution1 = uncurry solution1 <$> input

december05Solution2 :: IO String
december05Solution2 = uncurry solution2 <$> input
