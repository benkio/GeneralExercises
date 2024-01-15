module TwentyTwentyThree.TwelfthDecember where

import Data.List (groupBy, nub)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Debug.Trace

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

damageRecordToBlocks :: [Int] -> String -> ([[String]], [[Int]])
damageRecordToBlocks bl dr
    | nsl == bll = ([naturalSplit], [bl])
    | nsl > bll = ([naturalSplit], expandDamagedGroup (nsl - bll) bl)
    | otherwise = (nub (expandGroup (bll - nsl) naturalSplit), [bl])
  where
    naturalSplit = filter (not . null) $ splitOn "." dr
    nsl = length naturalSplit
    bll = length bl

expandDamagedGroup :: Int -> [Int] -> [[Int]]
expandDamagedGroup 0 xs = [xs]
expandDamagedGroup 1 xs = expandDamagedGroup' [] xs
expandDamagedGroup n xs = nub $ concatMap (expandDamagedGroup (n - 1)) $ expandDamagedGroup' [] xs

expandDamagedGroup' _ [] = []
expandDamagedGroup' [] (x : xs) = ([0, x] ++ xs) : expandDamagedGroup' [x] xs ++ [[x] ++ xs ++ [0]]
expandDamagedGroup' prefix (x : xs) = (prefix ++ [0, x] ++ xs) : expandDamagedGroup' (prefix ++ [x]) xs

expandGroup :: Int -> [String] -> [[String]]
expandGroup 0 xs = [xs]
expandGroup 1 xs = expandGroupOne xs
expandGroup n xs = concatMap (expandGroup (n - 1)) $ expandGroupOne xs

expandGroupOne :: [String] -> [[String]]
expandGroupOne s = go [] s
  where
    go _ [] = []
    go prefix (x : xs) = fmap (\y -> prefix ++ y ++ xs) (splitByQuestionmark x) ++ go (prefix ++ [x]) xs

splitByQuestionmark :: String -> [[String]]
splitByQuestionmark = filter ("" `notElem`) . map (splitOn ".") . questionMarkToPoint

questionMarkToPoint :: String -> [String]
questionMarkToPoint s = go [] s
  where
    go _ [] = []
    go prefix ('?' : xs) = (prefix ++ "." ++ xs) : go (prefix ++ "?") xs
    go prefix (x : xs) = go (prefix ++ [x]) xs

singleGroupCombination :: String -> Int -> Maybe [String]
singleGroupCombination xs n
    | length xs == n = Just [replicate (length xs) '#']
    | all (\x -> x == '?' || x == '.') xs && n == 0 = Just [replicate (length xs) '.']
    | length xs < n = Nothing
    | otherwise = Just $ filter ((== [replicate n '#']) . filter (not . null) . splitOn ".") $ combinations [] xs
  where
    combinations :: [String] -> String -> [String]
    combinations acc [] = acc
    combinations [] ('#' : ys) = combinations ["#"] ys
    combinations acc ('#' : ys) = combinations (fmap (++ "#") acc) ys
    combinations [] ('?' : ys) = combinations ["#", "."] ys
    combinations acc ('?' : ys) = combinations (concatMap (\x -> fmap (x ++) ["#", "."]) acc) ys

checkSpringRow :: SpringRows -> Int
checkSpringRow (SR{damagedRecord = dr, damagedGroups = dg}) = (length . nub . concat) combinations -- (length . nub . concat) $ do
-- bls <- damagedRecordBlocks
-- dg' <- damagedGroups'
-- let combinationGroups = -- traceShowId $
--       zipWith singleGroupCombination bls dg'
--     combination = -- traceShowId $
--       reconstructCombinations combinationGroups
-- return combination
-- (length . nub . concatMap reconstructCombinations . traceShowId . fmap (\bls ->
--         zipWith singleGroupCombination bls dg
--         )) damagedRecordBlocks
  where
    (damagedRecordBlocks, damagedGroups') = damageRecordToBlocks dg dr
    checkCombinations = concatMap (\bls -> mapMaybe (sequence . zipWith singleGroupCombination bls) damagedGroups') damagedRecordBlocks
    combinations = fmap reconstructCombinations checkCombinations

reconstructCombinations :: [[String]] -> [String]
reconstructCombinations [] = []
reconstructCombinations (xs : xss) = go xs xss
  where
    go :: [String] -> [[String]] -> [String]
    go acc [] = acc
    go acc ([] : yss) = go (fmap (++ ".") acc) yss
    go acc (ys : yss) = go (concatMap (\y -> fmap (++ "." ++ y) acc) ys) yss

solution1 :: [SpringRows] -> Int
solution1 =
    sum
        . traceShowId
        . fmap checkSpringRow

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution1 <$> input

solution2 = undefined

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = undefined

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
