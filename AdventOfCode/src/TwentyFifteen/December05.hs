module TwentyFifteen.December05 where

import Data.List (group, isInfixOf, sort)

input :: IO String
input = readFile "input/2015/5December.txt"

inputTest :: String
inputTest =
    "ugknbfddgicrmopn\n\
    \aaa\n\
    \jchzalrnumimnmhp\n\
    \haegwjzuvuyypxyu\n\
    \dvszwmarrgswjxmb"

testSolution1 :: Bool
testSolution1 =
    ((\l -> l == [True, True, False, False, False]) . solution1 . lines) inputTest

solution1 :: [String] -> [Bool]
solution1 = fmap isNice

isNice :: String -> Bool
isNice s
    | hasThreeVowels s
        && hasTwoAdjacentLetters s
        && doesNotContainForbiddenSequences s =
        True
    | otherwise = False

hasThreeVowels :: String -> Bool
hasThreeVowels =
    (>= 3)
        . foldl
            ( \num x ->
                if x `elem` "aeiou"
                    then num + 1
                    else num
            )
            0

hasTwoAdjacentLetters :: String -> Bool
hasTwoAdjacentLetters [] = False
hasTwoAdjacentLetters (x : xs) = findAdjacentLetter xs x
  where
    findAdjacentLetter :: String -> Char -> Bool
    findAdjacentLetter [] _ = False
    findAdjacentLetter (y : ys) c = (c == y) || findAdjacentLetter ys y

doesNotContainForbiddenSequences :: String -> Bool
doesNotContainForbiddenSequences s =
    foldl
        (\acc forbidden -> acc && not (forbidden `isInfixOf` s))
        True
        ["ab", "cd", "pq", "xy"]

december05Solution1 :: IO Int
december05Solution1 = length . filter id . solution1 . lines <$> input

solution2 :: [String] -> [Bool]
solution2 = fmap isNice2

isNice2 :: String -> Bool
isNice2 s
    | equalLetterWithOneInBetween s && twoPairNoOverlap s = True
    | otherwise = False

equalLetterWithOneInBetween :: String -> Bool
equalLetterWithOneInBetween s =
    foldl (\acc (x, _, y) -> x == y || acc) False $
        zip3 s (tail s) (tail (tail s))

twoPairNoOverlap :: String -> Bool
twoPairNoOverlap [] = False
twoPairNoOverlap s =
    let (p, rest) = splitAt 2 s
     in p `isInfixOf` rest || twoPairNoOverlap (tail s)

inputTest2 :: String
inputTest2 =
    "qjhvhtzxzqqjkmpb\n\
    \xxyxx\n\
    \uurcxstgmygtbstg\n\
    \ieodomkazucvgmuy"

testSolution2 :: Bool
testSolution2 =
    ((\l -> l == [True, True, False, False]) . solution2 . lines) inputTest2

december05Solution2 :: IO Int
december05Solution2 = length . filter id . solution2 . lines <$> input
