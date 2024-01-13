module TwentyTwentyThree.ThirteenthDecember where

import Data.Bifunctor (bimap)
import Data.List (find, groupBy, transpose)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

type Field = [String]

input :: IO [Field]
input = parseInput <$> readFile "input/2023/13December.txt"

parseInput :: String -> [Field]
parseInput = filter ((/= 1) . length) . groupBy (\l l' -> l /= "" && l' /= "") . lines

testInput :: [Field]
testInput =
    parseInput
        "#.##..##.\n\
        \..#.##.#.\n\
        \##......#\n\
        \##......#\n\
        \..#.##.#.\n\
        \..##..##.\n\
        \#.#.##.#.\n\
        \\n\
        \#...##..#\n\
        \#....#..#\n\
        \..##..###\n\
        \#####.##.\n\
        \#####.##.\n\
        \..##..###\n\
        \#....#..#"

findHReflection :: Field -> Maybe (Field, Field)
findHReflection f = listToMaybe $ mapMaybe (checkMirrorPair f pairs) mirrorPairs
  where
    pairs = zip f (tail f)
    mirrorPairs = filter (\(t, b) -> t == b) pairs

checkMirrorPair :: Field -> [(String, String)] -> (String, String) -> Maybe (Field, Field)
checkMirrorPair f pairs mirrorPair =
    let topReflection = ((++ [fst mirrorPair]) . fmap fst . takeWhile (/= mirrorPair)) pairs
        bottomReflection = ((++ [last f]) . fmap fst . drop 1 . dropWhile (/= mirrorPair)) pairs
        isReflection = all (\(a, b) -> a == b) $ zip (reverse topReflection) bottomReflection
     in if isReflection then Just (topReflection, bottomReflection) else Nothing

calculateH :: Field -> Maybe Int
calculateH f = do
    (t, _) <- findHReflection f
    return (100 * length t)

calculateV :: Field -> Maybe Int
calculateV f = do
    (l, _) <- findHReflection $ transpose f
    return (length l)

solution1 :: [Field] -> Int
solution1 =
    sum
        . fmap (\f -> fromMaybe 0 (calculateH f) + fromMaybe 0 (calculateV f))

-- 22288 too low
thirdteenthDecemberSolution1 :: IO Int
thirdteenthDecemberSolution1 = solution1 <$> input

solution2 = undefined

thirdteenthDecemberSolution2 :: IO Int
thirdteenthDecemberSolution2 = undefined
