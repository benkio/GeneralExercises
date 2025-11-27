module TwentyTwentyThree.December04 where

import Data.List
import qualified Data.Map as M
import Data.Maybe

input :: IO [Card]
input = parseInput <$> readFile "input/2023/4December.txt"

data Card = C {cId :: Int, winningNums :: [Int], nums :: [Int]}

parseInput :: String -> [Card]
parseInput = fmap (\(i, l) -> (parseCard i . drop 2 . dropWhile (/= ':')) l) . zip [1 ..] . lines
  where
    parseCard i s = C{cId = i, winningNums = (fmap (\x -> read x :: Int) . takeWhile (/= "|") . words) s, nums = (fmap (\x -> read x :: Int) . tail . dropWhile (/= "|") . words) s}

testInput :: [Card]
testInput =
    parseInput
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
        \Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
        \Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
        \Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
        \Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
        \Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

calcPoints :: (Int -> Int) -> Card -> Int
calcPoints f (C{winningNums = ws, nums = ns}) = foldl calcSinglePoints 0 ns
  where
    calcSinglePoints :: Int -> Int -> Int
    calcSinglePoints p n
        | n `elem` ws && p == 0 = 1
        | n `elem` ws && p /= 0 = f p
        | otherwise = p

solution1 :: [Card] -> Int
solution1 = sum . fmap (calcPoints (* 2))

december04Solution1 :: IO Int
december04Solution1 = solution1 <$> input

type Deck = M.Map Int (Int, Card)

initialDeck :: [Card] -> Deck
initialDeck = M.fromList . fmap (\c -> (cId c, (1, c)))

winScratchCards :: Int -> Deck -> Deck
winScratchCards i m
    | isNothing (M.lookup i m) = m
    | otherwise = winScratchCards (i + 1) nm
  where
    (n, c) = fromJust $ M.lookup i m
    nm = foldl (\m' (i, cs) -> M.adjust (\(x, c') -> (x + cs, c')) i m') m scratchCardCopies
    matches = calcPoints (+ 1) c
    scratchCardCopies = (fmap (\l -> (head l, length l)) . group . sort . concat . replicate n) [(((+ 1) . cId) c) .. (((+ matches) . cId) c)]

solution2 :: [Card] -> Int
solution2 = M.foldr (\(x, _) acc -> acc + x) 0 . winScratchCards 1 . initialDeck

december04Solution2 :: IO Int
december04Solution2 = solution2 <$> input
