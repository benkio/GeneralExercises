module TwentyTwentyTwo.EleventhDecember where

import Data.Bifunctor
import Data.List (groupBy, sort, sortBy)
import Data.Map (Map, adjust, elems, fromList, insert, size, (!))
import Data.Ord
import Debug.Trace
import Text.Printf

data Monkey = Monkey
    { index :: Int
    , inspectionCount :: Int
    , items :: [Int]
    , divisor :: Int
    , operation :: Int -> Int
    , test :: Int -> Int
    }

instance Show Monkey where
    show (Monkey{index = i, inspectionCount = isc, items = is}) = printf "Monkey %d -> Count: %d - Items: %s " i isc (show is)

input :: IO (Map Int Monkey)
input = parseInput <$> readFile "input/2022/11December.txt"

monkeyRound :: (Int -> Int) -> Int -> Map Int Monkey -> Map Int Monkey
monkeyRound transform i ms =
    let monkey = ms ! i
        inspectedIncrement = (length . items) monkey
        is = items monkey
        op = operation monkey
        testF = test monkey
        itemsInpected = fmap ((\i -> (i, testF i)) . transform . op) is
        monkey' = monkey{inspectionCount = inspectionCount monkey + inspectedIncrement, items = []}
     in insert i monkey' $ sendItems ms itemsInpected

sendItems :: Map Int Monkey -> [(Int, Int)] -> Map Int Monkey
sendItems = foldl (\acc (i, index) -> adjust (\m -> m{items = items m ++ [i]}) index acc)

roundMk :: (Int -> Int) -> Map Int Monkey -> Map Int Monkey
roundMk transform m = foldl (flip (monkeyRound transform)) m [0 .. (size m - 1)]

boredTransform :: Int -> Int
boredTransform x = div x 3

takeRound :: (Int -> Int) -> Int -> Map Int Monkey -> Map Int Monkey
takeRound transform i = (!! i) . iterate (roundMk transform)

solution1 :: Map Int Monkey -> Int
solution1 = product . take 2 . sortBy (comparing Data.Ord.Down) . fmap inspectionCount . elems . takeRound boredTransform 20

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = solution1 <$> input

solution2 :: Map Int Monkey -> Int
solution2 ms =
    ( product
        . take 2
        . sortBy (comparing Data.Ord.Down)
        . fmap inspectionCount
        . elems
        . takeRound transform 10000
    )
        ms
  where
    transform = (flip mod . product . fmap divisor . elems) ms

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = solution2 <$> input

-- Parsing ----------------------------------------

splitMonkeys :: String -> [[String]]
splitMonkeys = fmap (filter (not . null)) . groupBy (\_ s -> (not . null) s) . lines

takeNewLine :: String -> (String, String)
takeNewLine = second tail . break (== '\n')

takeColon :: String -> String
takeColon = tail . tail . dropWhile (/= ':')

toIntList :: String -> [Int]
toIntList [] = []
toIntList (',' : ' ' : xs) = toIntList xs
toIntList s = ((\(x, y) -> x : toIntList y) . first (\x -> read x :: Int) . break (== ',')) s

toOperation :: String -> Int -> Int
toOperation ('o' : 'l' : 'd' : xs) = toOperation xs
toOperation ('*' : ' ' : xs) = if xs == "old" then (\x -> x * x) else (\x -> x * (read xs :: Int))
toOperation ('+' : ' ' : xs) = if xs == "old" then (\x -> x + x) else (\x -> x + (read xs :: Int))

parseMonkeyItems :: String -> [Int]
parseMonkeyItems = toIntList . takeColon

parseMonkeyOperation :: String -> Int -> Int
parseMonkeyOperation = toOperation . drop 10 . takeColon

parseMonkeyTest :: [String] -> (Int -> Int, Int)
parseMonkeyTest s =
    let divisor = ((\x -> read x :: Int) . reverse . takeWhile (/= ' ') . reverse) (head s)
        monkeyTrue = ((\x -> read x :: Int) . reverse . takeWhile (/= ' ') . reverse) (s !! 1)
        monkeyFalse = ((\x -> read x :: Int) . reverse . takeWhile (/= ' ') . reverse) (s !! 2)
        test x = if (x `mod` divisor) == 0 then monkeyTrue else monkeyFalse
     in (test, divisor)

parseIndex :: String -> Int
parseIndex = (\x -> read [x] :: Int) . head . tail . dropWhile (/= ' ')

parseMonkey :: [String] -> Monkey
parseMonkey x =
    let index = parseIndex (head x)
        items = parseMonkeyItems (x !! 1)
        operation = parseMonkeyOperation (x !! 2)
        (test, divisor) = parseMonkeyTest (drop 3 x)
     in Monkey{index = index, divisor = divisor, inspectionCount = 0, items = items, operation = operation, test = test}

parseInput :: String -> Map Int Monkey
parseInput = fromList . fmap ((\m -> (index m, m)) . parseMonkey) . splitMonkeys

testInput :: Map Int Monkey
testInput =
    parseInput
        "Monkey 0:\n\
        \  Starting items: 79, 98\n\
        \  Operation: new = old * 19\n\
        \  Test: divisible by 23\n\
        \    If true: throw to monkey 2\n\
        \    If false: throw to monkey 3\n\
        \\n\
        \Monkey 1:\n\
        \  Starting items: 54, 65, 75, 74\n\
        \  Operation: new = old + 6\n\
        \  Test: divisible by 19\n\
        \    If true: throw to monkey 2\n\
        \    If false: throw to monkey 0\n\
        \\n\
        \Monkey 2:\n\
        \  Starting items: 79, 60, 97\n\
        \  Operation: new = old * old\n\
        \  Test: divisible by 13\n\
        \    If true: throw to monkey 1\n\
        \    If false: throw to monkey 3\n\
        \\n\
        \Monkey 3:\n\
        \  Starting items: 74\n\
        \  Operation: new = old + 3\n\
        \  Test: divisible by 17\n\
        \    If true: throw to monkey 0\n\
        \    If false: throw to monkey 1"
