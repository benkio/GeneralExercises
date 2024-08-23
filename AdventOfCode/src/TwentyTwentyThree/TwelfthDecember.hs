module TwentyTwentyThree.TwelfthDecember where

import Data.Bifunctor (second)
import Data.List (group, intercalate, intersperse)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert)
import qualified Data.Map as M (lookup)
import Debug.Trace

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

solution :: [SpringRows] -> Int
solution =
    snd
        . foldl
            ( \(mem, v) sr ->
                let (mem', v') = trace ("next " ++ show (damagedRecord sr) ++ " - " ++ show (damagedGroups sr)) groupCheck mem (damagedRecord sr) (damagedGroups sr)
                 in (mem', v + v')
            )
            (empty, 0)

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution <$> input

factorByFive :: SpringRows -> SpringRows
factorByFive s = s{damagedRecord = (intercalate "?" . replicate 5) (damagedRecord s), damagedGroups = (concat . replicate 5) (damagedGroups s)}

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

groupCheck :: Map (String, [Int]) Int -> String -> [Int] -> (Map (String, [Int]) Int, Int)
groupCheck mem [] [] = (mem, 1)
groupCheck mem [] _ = (mem, 0)
groupCheck mem s [] = if '#' `elem` s then (mem, 0) else (mem, 1)
groupCheck mem s xs =
    case M.lookup (s, xs) mem of
        Nothing -> (\(m, v) -> (insert (s, xs) v m, v)) (foldl foldlMemF (mem, 0) matchesAndRests)
        Just v -> (mem, v)
  where
    recordStripDot = stripDot s
    matchesAndRests = singleGroupCheck recordStripDot xs
    foldlMemF (mem', v') (_, rs) =
        let (mem'', result) = groupCheck mem' rs (tail xs)
         in (insert (rs, tail xs) result mem'', v' + result)

-- addMatch x = if null m then x else m ++ "." ++ x

singleGroupCheck :: String -> [Int] -> [(String, String)]
singleGroupCheck s [] = [(replicate (length s) '.', "") | '#' `notElem` s]
singleGroupCheck s xs = matchesAndRests
  where
    (matchingRecord, rest) = cropToMatchingSpringRegion s xs
    (s' : _) = groupToSpring xs
    matchesAndRests = second (drop 1) <$> filter (\(_, ys) -> null ys || (head ys `elem` ".?" && length ys >= minimalGroupLength (tail xs))) (second (++ rest) <$> matchSpringRecord matchingRecord s')

minimalGroupLength :: [Int] -> Int
minimalGroupLength [x] = x + 1
minimalGroupLength xs = ((+ (length xs - 1)) . sum) xs
cropToMatchingSpringRegion :: String -> [Int] -> (String, String)
cropToMatchingSpringRegion s xs = splitAt (length s - minimalGroupLength (tail xs)) s

groupToSpring :: [Int] -> [String]
groupToSpring = intersperse "." . fmap (`replicate` '#')

paddingLeftWithDots :: Int -> String -> String
paddingLeftWithDots i s = replicate i '.' ++ s

paddingRightWithDotsTillLength :: Int -> String -> String
paddingRightWithDotsTillLength l s = s ++ replicate (l - length s) '.'

matchSpringRecord :: String -> String -> [(String, String)]
matchSpringRecord r s = result
  where
    matchF x = and $ zipWith compareSpring r x
    matches = filter matchF $ fmap (`paddingLeftWithDots` s) [0 .. (length r - length s)]
    result = fmap (\m -> (m, drop (length m) r)) matches

compareSpring :: Char -> Char -> Bool
compareSpring '#' '#' = True
compareSpring '.' '.' = True
compareSpring _ '?' = True
compareSpring '?' _ = True
compareSpring x y = False

stripDot :: String -> String
stripDot = concatMap (\xs -> if head xs == '.' then "." else xs) . group
