module TwentyTwentyThree.December07 where

import Data.Bifunctor (first)
import Data.List (elemIndex, group, nub, nubBy, sort, sortOn)

newtype Hand = H String deriving (Eq, Show)

instance Ord Hand where
    compare h h' = case handType h `compare` handType h' of
        EQ -> handComparisonByHighCard cards h h'
        LT -> LT
        GT -> GT

input :: IO [(Hand, Int)]
input = parseInput <$> readFile "input/2023/7December.txt"

cards :: String
cards = "AKQJT98765432"

handType :: Hand -> Int
handType (H s) = (handType' . group . sort) s
  where
    handType' :: [String] -> Int
    handType' xs
        | length xs == 1 = 7
        | length xs == 2 && 4 `elem` fmap length xs = 6
        | length xs == 2 && 3 `elem` fmap length xs = 5
        | length xs == 3 && 3 `elem` fmap length xs = 4
        | length xs == 3 && 2 `elem` fmap length xs = 3
        | length xs == 4 = 2
        | length xs == 5 = 1

handComparisonByHighCard :: String -> Hand -> Hand -> Ordering
handComparisonByHighCard cards' (H h) (H h') = highCardCompare $ h `zip` h'
  where
    highCardCompare :: [(Char, Char)] -> Ordering
    highCardCompare [] = EQ
    highCardCompare ((c, c') : cs)
        | c == c' = highCardCompare cs
        | elemIndex c cards' > elemIndex c' cards' = LT
        | elemIndex c' cards' > elemIndex c cards' = GT

parseInput :: String -> [(Hand, Int)]
parseInput = fmap ((\[h, x] -> (H h, read x :: Int)) . words) . lines

testInput :: [(Hand, Int)]
testInput =
    parseInput
        "32T3K 765\n\
        \T55J5 684\n\
        \KK677 28\n\
        \KTJJT 220\n\
        \QQQJA 483"

solution1 :: [(Hand, Int)] -> Int
solution1 = sum . fmap (\(r, (_, b)) -> r * b) . zip [1 ..] . sortOn fst

newtype HandJ = HJ String deriving (Eq, Show)

cardsJ :: String
cardsJ = "AKQT98765432J"

handTypeJ :: HandJ -> Int
handTypeJ = maximum . fmap handType . genHandJ

genHandJ :: HandJ -> [Hand]
genHandJ (HJ h) = fmap H . nubBy (\s s' -> sort s == sort s') $ go noJs ((length . filter (== 'J')) h)
  where
    noJs = filter (/= 'J') h
    go :: String -> Int -> [String]
    go acc 0 = [acc]
    go acc js = do
        c <- 'J' : nub noJs
        go (c : acc) (js - 1)

instance Ord HandJ where
    compare h@(HJ s) h'@(HJ s') = case handTypeJ h `compare` handTypeJ h' of
        EQ -> handComparisonByHighCard cardsJ (H s) (H s')
        LT -> LT
        GT -> GT

december07Solution1 :: IO Int
december07Solution1 = solution1 <$> input

solution2 :: [(Hand, Int)] -> Int
solution2 = sum . fmap (\(r, (_, b)) -> r * b) . zip [1 ..] . sortOn fst . fmap (first (\(H s) -> HJ s))

december07Solution2 :: IO Int
december07Solution2 = solution2 <$> input
