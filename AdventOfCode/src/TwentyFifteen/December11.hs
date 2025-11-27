module TwentyFifteen.December11 where

import Data.List
import Data.Map (Map)
import Data.Map as Map (fromList, lookup, toList)
import Data.Maybe

input :: IO String
input = filter (/= '\n') <$> readFile "input/2015/11December.txt"

alphabetToNums :: Map Char Int
alphabetToNums = Map.fromList $ ['a' .. 'z'] `zip` [1 ..]

stringToNumbers :: String -> [Int]
stringToNumbers = fmap (\c -> fromJust (Map.lookup c alphabetToNums))

numbersToString :: [Int] -> String
numbersToString [] = []
numbersToString xs =
    map
        ( \x ->
            (fst . fromJust . find (\(_, v) -> v == x) . Map.toList) alphabetToNums
        )
        xs

zip3List :: [Int] -> [[Int]]
zip3List xs =
    let pairs = zip3 xs (tail xs) (tail (tail xs))
     in fmap (\(a, b, c) -> [a, b, c]) pairs

sequenceOfThreeConsecutive :: [[Int]]
sequenceOfThreeConsecutive = zip3List [1 .. 26]

hasThreeConsecutive :: [Int] -> Bool
hasThreeConsecutive xs = any (`elem` sequenceOfThreeConsecutive) (zip3List xs)

hasForbiddenLetters :: [Int] -> Bool
hasForbiddenLetters = any (`elem` stringToNumbers "iol")

hasTwoDifferentNonOverlappingPairs :: [Int] -> Bool
hasTwoDifferentNonOverlappingPairs =
    (2 <=) . length . filter ((1 <) . length) . group

generateNewPassword :: [Int] -> [Int]
generateNewPassword =
    ( \(l, r) ->
        if r == 1
            then 1 : l
            else l
    )
        . foldr moveLastChar ([], 1)
  where
    moveLastChar :: Int -> ([Int], Int) -> ([Int], Int)
    moveLastChar x (acc, r) =
        let (x', r') =
                if (x + r) `elem` [1 .. 26]
                    then (x + r, 0)
                    else (1, 1)
         in (x' : acc, r')

generateNewValidPassword :: [Int] -> [Int]
generateNewValidPassword xs
    | hasTwoDifferentNonOverlappingPairs xs
        && hasThreeConsecutive xs
        && not (hasForbiddenLetters xs) =
        xs
    | otherwise = generateNewValidPassword $ generateNewPassword xs

inputTest :: [String]
inputTest = ["abcdefgh", "ghijklmn"]

solution1Test :: Bool
solution1Test = ["abcdffaa", "ghjaabcc"] == fmap solution inputTest

solution :: String -> String
solution =
    numbersToString
        . generateNewValidPassword
        . generateNewPassword
        . stringToNumbers

december11Solution1 :: IO String
december11Solution1 = solution <$> input

december11Solution2 :: IO String
december11Solution2 = solution <$> december11Solution1
