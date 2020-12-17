-------------------------------------------------------------------------------
--                           Advent Of Code - day 16                          --
-------------------------------------------------------------------------------
module TwentyTwenty.SixteenthDecember where

import           Data.List       (delete, isPrefixOf, transpose)
import           Text.Regex.TDFA

data Rule =
  Rule
    { ruleName   :: String
    , ruleRange1 :: (Int, Int)
    , ruleRange2 :: (Int, Int)
    }
  deriving (Show, Eq)

type Ticket = [Int]

input :: IO String
input = readFile "input/2020/16December.txt"

parseRuleInput :: String -> Rule
parseRuleInput s =
  let (rName, _, ruleRanges) = s =~ ": " :: (String, String, String)
      (range1, _, range2) = ruleRanges =~ " or " :: (String, String, String)
      (r1Range1, r1Range2) = span ('-' /=) range1
      (r2Range1, r2Range2) = span ('-' /=) range2
   in Rule
        { ruleName = rName
        , ruleRange1 = (read r1Range1 :: Int, read (tail r1Range2) :: Int)
        , ruleRange2 = (read r2Range1 :: Int, read (tail r2Range2) :: Int)
        }

parseTicket :: String -> Ticket
parseTicket =
  fmap (\x -> read x :: Int) .
  foldl
    (\acc c ->
       if c == ','
         then acc ++ [""]
         else init acc ++ [last acc ++ [c]])
    [""]

parseInput :: String -> ([Rule], Ticket, [Ticket])
parseInput s =
  let (stringRules, _, rest) =
        s =~ "\nyour ticket:\n" :: (String, String, String)
      rules = (fmap parseRuleInput . lines) stringRules
      (myTicketString, _, nearbyTicketsString) =
        rest =~ "\n\nnearby tickets:\n" :: (String, String, String)
      myTicket = parseTicket myTicketString
      nearbyTickets = (fmap parseTicket . lines) nearbyTicketsString
   in (rules, myTicket, nearbyTickets)

reduceRules :: [Rule] -> [(Int, Int)]
reduceRules [] = []
reduceRules xs = foldl collectRules [] xs
  where
    collectRules :: [(Int, Int)] -> Rule -> [(Int, Int)]
    collectRules ranges Rule {ruleRange1 = rr1, ruleRange2 = rr2} =
      ranges ++ [rr1, rr2]

applyRules :: Ticket -> [(Int, Int)] -> [Int]
applyRules t r = foldl (checkValue r) [] t
  where
    checkValue :: [(Int, Int)] -> [Int] -> Int -> [Int]
    checkValue ranges invalidValues v
      | any (\(rrl, rrh) -> v >= rrl && v <= rrh) ranges = invalidValues
      | otherwise = invalidValues ++ [v]

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 =
  sum .
  (\(rules, _, nearbyTickets) ->
     nearbyTickets >>= (\t -> applyRules t (reduceRules rules))) .
  parseInput <$>
  input

isValidRule :: Ticket -> Rule -> Bool
isValidRule [] _ = True
isValidRule (v:vs) r@Rule {ruleRange1 = (rrll, rrlh), ruleRange2 = (rrhl, rrhh)} =
  (v >= rrll && v <= rrlh || v >= rrhl && v <= rrhh) && isValidRule vs r

reorderRules :: [Rule] -> [(Int, Ticket)] -> [(Int, Rule)]
reorderRules _ [] = []
reorderRules rs ((i, t):ts) =
  let (r:rs') =
        foldl
          (\acc x ->
             if isValidRule t x
               then acc ++ [x]
               else acc)
          []
          rs
      rulesWithoutRule = delete r rs
   in if null rs'
        then reorderRules rulesWithoutRule ts ++ [(i, r)]
        else reorderRules (rulesWithoutRule ++ [r]) (ts ++ [(i, t)])

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 =
  (\(rules, myTicket, nearbyTickets) ->
     (product .
      fmap ((myTicket !!) . fst) .
      filter (\(_, r) -> "departure" `isPrefixOf` ruleName r) .
      reorderRules rules .
      zip [0 ..] .
      transpose . filter (\t -> null (applyRules t (reduceRules rules))))
       nearbyTickets) .
  parseInput <$>
  input

testInput :: String
testInput =
  "class: 0-1 or 4-19\n\
\row: 0-5 or 8-19\n\
\seat: 0-13 or 16-19\n\
\\n\
\your ticket:\n\
\11,12,13\n\
\\n\
\nearby tickets:\n\
\3,9,18\n\
\15,1,5\n\
\5,14,9 "
