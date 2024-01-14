module TwentyTwentyThree.ThirteenthDecember where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap)
import Data.List (delete, find, groupBy, transpose)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Debug.Trace

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

solution :: (Field -> Field) -> [Field] -> Int
solution f =
    sum
        . traceShowId
        . fmap (calculateReflection . f)
  where
    calculateReflection f = fromMaybe 0 $ (calculateH f) <|> (calculateV f)

thirdteenthDecemberSolution1 :: IO Int
thirdteenthDecemberSolution1 = solution id <$> input

solution2 :: [Field] -> Int
solution2 = sum . mapMaybe (\f -> findSmudgeIndex f calcH <|> findSmudgeIndex (transpose f) calcV)
  where calcH i = 100 * i
        calcV = id

findSmudgeIndex :: Field -> (Int -> Int) -> Maybe Int
findSmudgeIndex field f = (fmap f . find checkIndex) [1..length field - 1]
  where
    topReflection i = take i field
    bottomReflection i = drop i field
    checkIndex i = ((==1) . sum) $ (\(l,l') -> (length . filter (==False) . fmap (uncurry (==))) (zip l l'))  <$> zip (reverse (topReflection i)) (bottomReflection i)

-- right answer: 25401
-- [9, 10, 7, 100, 1400, 1, 500, 6, 7, 6, 1300, 300, 900, 1, 100, 200, 800, 1, 500, 2, 800, 8, 9, 700, 9, 6, 1200, 1500, 12, 5, 100, 6, 10, 9, 200, 1300, 100, 500, 600, 12, 9, 100, 100, 12, 500, 9, 400, 600, 1, 1, 1200, 200, 8, 100, 7, 500, 800, 12, 800, 500, 200, 12, 4, 5, 500, 4, 800, 16, 5, 6, 500, 1, 13, 200, 400, 800, 3, 3, 700, 12, 9, 800, 8, 9, 9, 3, 200, 500, 5, 500, 16, 3, 12, 3, 8, 7, 7, 14, 8, 1]
thirdteenthDecemberSolution2 :: IO Int
thirdteenthDecemberSolution2 = solution2 <$> input
