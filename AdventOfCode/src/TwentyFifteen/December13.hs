module TwentyFifteen.December13 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type HappinessMap = Map (String, String) Int

inputTest :: HappinessMap
inputTest =
    (Map.fromList . fmap parseHappiness . lines)
        "Alice would gain 54 happiness units by sitting next to Bob.\n\
        \Alice would lose 79 happiness units by sitting next to Carol.\n\
        \Alice would lose 2 happiness units by sitting next to David.\n\
        \Bob would gain 83 happiness units by sitting next to Alice.\n\
        \Bob would lose 7 happiness units by sitting next to Carol.\n\
        \Bob would lose 63 happiness units by sitting next to David.\n\
        \Carol would lose 62 happiness units by sitting next to Alice.\n\
        \Carol would gain 60 happiness units by sitting next to Bob.\n\
        \Carol would gain 55 happiness units by sitting next to David.\n\
        \David would gain 46 happiness units by sitting next to Alice.\n\
        \David would lose 7 happiness units by sitting next to Bob.\n\
        \David would gain 41 happiness units by sitting next to Carol."

input :: IO HappinessMap
input =
    Map.fromList . fmap parseHappiness . lines
        <$> readFile "input/2015/13December.txt"

parseHappiness :: String -> ((String, String), Int)
parseHappiness s =
    let ws = words s
        persons = (head ws, init (last ws))
        (gainOrLose, value) = (ws !! 2, read (ws !! 3) :: Int)
        value' =
            if gainOrLose == "lose"
                then negate value
                else value
     in (persons, value')

allPartecipants :: HappinessMap -> [String]
allPartecipants = nub . fmap fst . Map.keys

nextSitPairs :: [String] -> [(String, String)]
nextSitPairs s =
    let cs = cycle s
        pairs = cs `zip` tail cs
     in concatMap (\(a, b) -> [(a, b), (b, a)]) (take (length s) pairs)

calculateTotalHappiness :: HappinessMap -> [(String, String)] -> Int
calculateTotalHappiness m = sum . fmap (\k -> fromJust (Map.lookup k m))

solution1 :: HappinessMap -> Int
solution1 m =
    let partecipantsCombinations = permutations $ allPartecipants m
        partecipantsCombinations' = fmap nextSitPairs partecipantsCombinations
     in maximum $ fmap (calculateTotalHappiness m) partecipantsCombinations'

december13Solution1 :: IO Int
december13Solution1 = solution1 <$> input

generateMe :: HappinessMap -> HappinessMap
generateMe =
    Map.fromList
        . concatMap (\p -> [(("Me", p), 0), ((p, "Me"), 0)])
        . allPartecipants

solution2 :: HappinessMap -> Int
solution2 m =
    let allPartecipantsAndMe = m `Map.union` generateMe m
     in solution1 allPartecipantsAndMe

december13Solution2 :: IO Int
december13Solution2 = solution2 <$> input
