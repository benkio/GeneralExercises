module TwentyTwentyThree.TwelfthDecember where

import Data.List (intersperse, isPrefixOf, nub, permutations)
import Data.List.Split (splitOn)
import Data.Tree (Tree, foldTree, unfoldTree)
import Debug.Trace

data SpringRows = SR {damagedRecord :: String, damagedGroups :: [Int]} deriving (Show)

input :: IO [SpringRows]
input = parseInput <$> readFile "input/2023/12December.txt"

parseInput :: String -> [SpringRows]
parseInput = fmap parseSpringRow . lines
  where
    parseSpringRow = (\[dr, dg] -> SR{damagedRecord = dr, damagedGroups = (fmap (\x -> read x :: Int) . splitOn ",") dg}) . words

generateSpringsTree :: [Int] -> String -> Tree String
generateSpringsTree expectedGroup s = unfoldTree (generateSprings expectedGroup) s

generateSprings :: [Int] -> String -> (String, [String])
generateSprings _ [] = ([], [])
generateSprings expectedGroup s = (prefix, children)
  where
    (prefix, rest) = break (== '?') s
    childrenPrefix = filter filterChildren $ if null rest then [] else [prefix ++ "#", prefix ++ "."]
    filterChildren g = filterValidRecord expectedGroup $ (recordToGroup g)
    hasQuestions = (> 0) . length . filter (== '?')
    children = (filter (\g -> if hasQuestions g then True else recordToGroup g == expectedGroup) . fmap (++ drop 1 rest)) childrenPrefix

-- fixRecords :: [String] -> [Int] -> String -> [String]
-- fixRecords rs expectedGroup ('?' : xs) =
--     let
--         rs' = if null rs then ["#", "."] else concatMap (\r -> [r ++ ['#'], r ++ ['.']]) rs
--         nextRecords = filter ((filterValidRecord expectedGroup) . recordToGroup) rs'
--      in
--         fixRecords
--             ( -- traceShowId
--               nextRecords
--             )
--             expectedGroup
--             xs
-- fixRecords rs expectedGroup (x : xs) =
--     let
--         rs' = if null rs then [[x]] else fmap (\r -> r ++ [x]) rs
--         nextRecords = filter ((filterValidRecord expectedGroup) . recordToGroup) rs'
--      in
--         fixRecords
--             ( -- traceShowId
--               nextRecords
--             )
--             expectedGroup
--             xs
-- fixRecords rs expectedGroup [] = filter (((==) expectedGroup) . recordToGroup) rs

filterValidRecord :: [Int] -> [Int] -> Bool
filterValidRecord es [] = True
filterValidRecord [] (x : xs) = False
filterValidRecord (e : es) (x : []) = x <= e
filterValidRecord (e : es) (x : xs) = e == x && filterValidRecord es xs

-- filterValidRecord x y = error $ "WTF: " ++ (show x) ++ " - " ++ (show y)

recordToGroup :: String -> [Int]
recordToGroup = fmap length . filter (not . null) . splitOn "."

howManyValidCombinations :: [Int] -> String -> Int
howManyValidCombinations expectedGroup record = foldTree (\s cs -> if length s == length record && null cs then 1 else sum cs) $ generateSpringsTree expectedGroup record

solution :: ([Int] -> String -> Int) -> [SpringRows] -> Int
solution alg = sum . fmap solutionSpringRow
  where
    solutionSpringRow (SR{damagedRecord = dr, damagedGroups = dg}) =
        alg dg (removeConsecutiveDots dr)

testInput :: [SpringRows]
testInput =
    parseInput
        "???.### 1,1,3\n\
        \.??..??...?##. 1,1,3\n\
        \?#?#?#?#?#?#?#? 1,3,1,6\n\
        \????.#...#... 4,1,1\n\
        \????.######..#####. 1,6,5\n\
        \?###???????? 3,2,1"

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution howManyValidCombinations <$> input

factorBy5 :: SpringRows -> SpringRows
factorBy5 (SR{damagedRecord = dr, damagedGroups = dg}) = (SR{damagedRecord = f dr, damagedGroups = g dg})
  where
    f = concat . intersperse "?" . replicate 5
    g = concat . replicate 5

minimalBitMask :: [Int] -> [String]
minimalBitMask = intersperse "." . fmap (`replicate` '#')

bitMaskLength :: [String] -> Int
bitMaskLength = sum . fmap length

expandBitMaskTo :: Int -> [String] -> [String]
expandBitMaskTo size xs =
    (fmap concat . filter (\b -> (not . successiveOnes) b && recordToGroup (concat b) == recordToGroup (concat xs)) . nub) sequences
  where
    extraZeros = replicate (size - bitMaskLength xs) "."
    successiveOnes xs = any (\(a, b) -> '#' `elem` a && '#' `elem` b) $ zip xs (tail xs)
    sequences = if null extraZeros then permutations xs else permutations (extraZeros ++ xs)

matchBitMask :: String -> String -> Bool
matchBitMask [] [] = True
matchBitMask (x : xs) (b : bs)
    | x == '?' = True && matchBitMask xs bs
    | x == b = True && matchBitMask xs bs
    | otherwise = False

removeConsecutiveDots :: String -> String
removeConsecutiveDots (x : xs : xss)
    | x == xs && x == '.' = removeConsecutiveDots (xs : xss)
    | otherwise = x : removeConsecutiveDots (xs : xss)
removeConsecutiveDots s = s

-- howManyValidCombinations' :: [Int] -> String -> Int
howManyValidCombinations' expectedGroup record =
    length $
        traceShowId $
            filter
                (\b -> matchBitMask minimalRecord b)
                bitMasks
  where
    mBitMask = traceShowId $ minimalBitMask expectedGroup
    minimalRecord = traceShowId $ removeConsecutiveDots record
    bitMasks = expandBitMaskTo (length minimalRecord) mBitMask

solution2 = solution howManyValidCombinations . fmap factorBy5

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = solution2 <$> input
