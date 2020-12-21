-------------------------------------------------------------------------------
--                           Advent Of Code - day 19                          --
-------------------------------------------------------------------------------
module TwentyTwenty.NineteenthDecember where

import Data.Bifunctor (bimap, first, second)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, cartesianProduct, map, singleton, union)
import Prelude hiding (lookup, map)

data Rule
  = TerminalRule Int Char
  | OrRule Int [Int]
  | CompositeRule Int [Int] [Int]
  deriving (Show)

input :: IO String
input = readFile "input/2020/19December.txt"

ruleId :: Rule -> Int
ruleId (TerminalRule x _) = x
ruleId (OrRule x _) = x
ruleId (CompositeRule x _ _) = x

parseRule :: String -> Rule
parseRule s
  | '"' `elem` s =
    let (id', rest) = extractId s
        char = extractChar rest
     in TerminalRule (read id' :: Int) char
  | '|' `elem` s =
    let (id', rest) = extractId s
        (ids1, ids2) = extractInnerIds rest
     in CompositeRule (read id' :: Int) ids1 ids2
  | otherwise =
    let (id', rest) = extractId s
        ids = extractInnerId rest
     in OrRule (read id' :: Int) ids
  where
    extractId :: String -> (String, String)
    extractId = second (drop 2) . span (':' /=)
    extractChar :: String -> Char
    extractChar = head . tail . dropWhile ('"' /=)
    extractInnerId :: String -> [Int]
    extractInnerId = fmap (\x -> read x :: Int) . words
    extractInnerIds :: String -> ([Int], [Int])
    extractInnerIds =
      bimap
        (fmap (\x -> read x :: Int) . words)
        (fmap (\x -> read x :: Int) . words . tail) .
      span ('|' /=)

rulesToMap :: [Rule] -> Map Int Rule
rulesToMap = fromList . fmap (\x -> (ruleId x, x))

getRule :: Map Int Rule -> Int -> Rule
getRule m i = fromJust $ lookup i m

ruleToValidString :: Rule -> Map Int Rule -> Set String
ruleToValidString (TerminalRule _ c) _ = singleton [c]
ruleToValidString (OrRule _ rs) rules =
  foldl1 stringProduct $ ruleIdToString <$> rs
  where
    ruleIdToString :: Int -> Set String
    ruleIdToString = (`ruleToValidString` rules) . getRule rules
ruleToValidString (CompositeRule _ rs1 rs2) rules =
  foldl1 stringProduct (ruleIdToString <$> rs1) `union`
  foldl1 stringProduct (ruleIdToString <$> rs2)
  where
    ruleIdToString :: Int -> Set String
    ruleIdToString = (`ruleToValidString` rules) . getRule rules

stringProduct :: Set String -> Set String -> Set String
stringProduct s1 s2 = map (uncurry (++)) $ cartesianProduct s1 s2

solution1 :: String -> Int
solution1 s =
  let (rules, messages) =
        (bimap (rulesToMap . fmap parseRule) tail . span ("" /=) . lines) s
      validStrings = ruleToValidString (getRule rules 0) rules
   in length $ filter (`elem` validStrings) messages

testInput :: String
testInput =
  "0: 4 1 5\n\
\1: 2 3 | 3 2\n\
\2: 4 4 | 5 5\n\
\3: 4 5 | 5 4\n\
\4: \"a\"\n\
\5: \"b\"\n\
\\n\
\ababbb\n\
\bababa\n\
\abbbab\n\
\aaabbb\n\
\aaaabbb"

testInput2 :: String
testInput2 =
  "42: 9 14 | 10 1\n\
\9: 14 27 | 1 26\n\
\10: 23 14 | 28 1\n\
\1: \"a\"\n\
\11: 42 31\n\
\5: 1 14 | 15 1\n\
\19: 14 1 | 14 14\n\
\12: 24 14 | 19 1\n\
\16: 15 1 | 14 14\n\
\31: 14 17 | 1 13\n\
\6: 14 14 | 1 14\n\
\2: 1 24 | 14 4\n\
\0: 8 11\n\
\13: 14 3 | 1 12\n\
\15: 1 | 14\n\
\17: 14 2 | 1 7\n\
\23: 25 1 | 22 14\n\
\28: 16 1\n\
\4: 1 1\n\
\20: 14 14 | 1 15\n\
\3: 5 14 | 16 1\n\
\27: 1 6 | 14 18\n\
\14: \"b\"\n\
\21: 14 1 | 1 14\n\
\25: 1 1 | 1 14\n\
\22: 14 14\n\
\8: 42\n\
\26: 14 22 | 1 20\n\
\18: 15 15\n\
\7: 14 5 | 1 21\n\
\24: 14 1\n\
\\n\
\abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\n\
\bbabbbbaabaabba\n\
\babbbbaabbbbbabbbbbbaabaaabaaa\n\
\aaabbbbbbaaaabaababaabababbabaaabbababababaaa\n\
\bbbbbbbaaaabbbbaaabbabaaa\n\
\bbbababbbbaaaaaaaabbababaaababaabab\n\
\ababaaaaaabaaab\n\
\ababaaaaabbbaba\n\
\baabbaaaabbaaaababbaababb\n\
\abbbbabbbbaaaababbbbbbaaaababb\n\
\aaaaabbaabaaaaababaa\n\
\aaaabbaaaabbaaa\n\
\aaaabbaabbaaaaaaabbbabbbaaabbaabaaa\n\
\babaaabbbaaabaababbaabababaaab\n\
\aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n\
\aaaaababaababab"

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = solution1 <$> input

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = solition2 <$> input

removeRulePrefix :: Set String -> String -> (Int, String)
removeRulePrefix ruleSet s =
  let prefix = find (`isPrefixOf` s) ruleSet
   in foldr
        (\p s' ->
           first
             (+ 1)
             (removeRulePrefix ruleSet (fromJust (stripPrefix p (snd s')))))
        (0, s)
        prefix

solition2 :: String -> Int
solition2 s =
  let (rules, messages) =
        (bimap (rulesToMap . fmap parseRule) tail . span ("" /=) . lines) s
      validStrings = ruleToValidString (getRule rules 31) rules
      fortyTwoRule = ruleToValidString (getRule rules 42) rules
      removedFortyTwoRule =
        filter (\(b, x) -> not (null x) && b > 0) $
        fmap (removeRulePrefix fortyTwoRule) messages
      removedValidStrings =
        (filter (\(x, m) -> null m && x > 0) .
         fmap
           (\(countFortyTwo, m) ->
              let (countThirtyOne, m') = removeRulePrefix validStrings m
               in (countFortyTwo - countThirtyOne, m')))
          removedFortyTwoRule
   in length removedValidStrings
