module TwentyTwentyThree.TwelfthDecember where

import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Data.List (intersperse, group)
import Debug.Trace

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

solution :: [SpringRows] -> Int
solution = length . concatMap (\sr -> groupCheck "" (damagedRecord sr) (damagedGroups sr))

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution <$> input

factorByFive :: SpringRows -> SpringRows
factorByFive s = s { damagedRecord = (intersperse '?' . concat . replicate 5) (damagedRecord s), damagedGroups = (concat . replicate 5) (damagedGroups s) }

solution2 = solution . fmap factorByFive

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = solution2 <$> input

input :: IO [SpringRows]
input = parseInput <$> readFile "input/2023/12December.txt"

parseInput :: String -> [SpringRows]
parseInput = fmap parseSpringRow . lines
  where
    parseSpringRow = (\[dr, dg] -> SR{damagedRecord = dr, damagedGroups = (fmap (\x -> read x :: Int) . splitOn ",") dg}) . words

testInput :: [SpringRows]
testInput =
    parseInput
        "???.### 1,1,3\n\
        \.??..??...?##. 1,1,3\n\
        \?#?#?#?#?#?#?#? 1,3,1,6\n\
        \????.#...#... 4,1,1\n\
        \????.######..#####. 1,6,5\n\
        \?###???????? 3,2,1"

groupCheck :: String -> String -> [Int] -> [String]
groupCheck m [] [] = [m]
groupCheck _ [] _ = []
groupCheck m s [] = if any (=='#') s then [] else [m ++ replicate (length s) '.']
groupCheck m s xs = filter (not . null)  $ concatMap (\(m', rs) -> groupCheck (addMatch m') rs (tail xs)) matchesAndRests
  where
    recordStripDot = stripDot s
    matchesAndRests = singleGroupCheck recordStripDot xs
    addMatch x = if null m then x else m ++ "." ++ x

singleGroupCheck :: String -> [Int] -> [(String, String)]
singleGroupCheck s [] = if any (=='#') s then [] else [(replicate (length s) '.', "")]
singleGroupCheck s xs = matchesAndRests
  where
    (matchingRecord, rest) = cropToMatchingSpringRegion s xs
    (s':_) = groupToSpring xs
    matchesAndRests = fmap (second (drop 1)) $ filter (\(_, ys) -> null ys || (head ys `elem` ".?" && length ys >= minimalGroupLength (tail xs))) $ fmap (second (++ rest))$ matchSpringRecord matchingRecord s'

minimalGroupLength :: [Int] -> Int
minimalGroupLength (x:[]) = x + 1
minimalGroupLength xs = ((+ (length xs - 1)) . sum) xs
cropToMatchingSpringRegion :: String -> [Int] -> (String, String)
cropToMatchingSpringRegion s xs = splitAt (length s - (minimalGroupLength (tail xs))) s

groupToSpring :: [Int] -> [String]
groupToSpring = intersperse "." . fmap (`replicate` '#')

paddingLeftWithDots :: Int -> String -> String
paddingLeftWithDots i s = replicate i '.' ++ s

paddingRightWithDotsTillLength :: Int -> String -> String
paddingRightWithDotsTillLength l s = s ++ replicate (l - length s) '.'

matchSpringRecord :: String -> String -> [(String, String)]
matchSpringRecord r s = result
  where
    matchF x = all id $ zipWith compareSpring r x
    matches = filter matchF $ fmap (`paddingLeftWithDots` s) [0..(length r - length s)]
    result = fmap (\m -> (m, drop (length m) r)) matches

compareSpring :: Char -> Char -> Bool
compareSpring '#' '#' = True
compareSpring '.' '.' = True
compareSpring _ '?' = True
compareSpring '?' _ = True
compareSpring x y = False

stripDot :: String -> String
stripDot = concatMap (\xs -> if head xs == '.' then "." else xs) . group
