module TwentyTwentyOne.TwelfthDecember where

import Data.Bifunctor (second)
import Data.Char (isLower, isUpper)
import Data.List (group, groupBy, sort)
import Debug.Trace

data Cave = Big String [String] | Small String [String] | Start [String] | End [String] deriving (Show, Eq, Ord)

-- instance Show Cave where
--   show (Big n _) = n
--   show (Small n _) = n
--   show (Start _) = "start"
--   show (End _) = "end"

parseInput :: String -> [Cave]
parseInput =
    fmap (foldl1 aggregateCaves) . groupBy (\c c' -> getNameCave c == getNameCave c') . sort . concatMap parseConnection . lines
  where
    aggregateCaves :: Cave -> Cave -> Cave
    aggregateCaves c c'
        | getNameCave c == getNameCave c' = buildCave (getNameCave c) (getConnectionCave c ++ getConnectionCave c')
        | otherwise = error "Can't aggregate different caves together"

parseConnection :: String -> [Cave]
parseConnection s =
    let (c, c') = (second tail . break (== '-')) s
     in [buildCave c [c'], buildCave c' [c]]

getConnectionCave :: Cave -> [String]
getConnectionCave (Start c) = c
getConnectionCave (End c) = c
getConnectionCave (Small _ c) = c
getConnectionCave (Big _ c) = c

getNameCave :: Cave -> String
getNameCave (Start _) = "start"
getNameCave (End _) = "end"
getNameCave (Small n _) = n
getNameCave (Big n _) = n

isStart :: Cave -> Bool
isStart (Start _) = True
isStart _ = False

isEnd :: Cave -> Bool
isEnd (End _) = True
isEnd _ = False

isBig :: Cave -> Bool
isBig (Big _ _) = True
isBig _ = False

isSmall :: Cave -> Bool
isSmall (Small _ _) = True
isSmall _ = False

buildCave :: String -> [String] -> Cave
buildCave s cs
    | s == "start" = Start cs
    | s == "end" = End cs
    | all isLower s = Small s cs
    | all isUpper s = Big s cs
    | otherwise = error $ "Unreachable: buildCave " ++ s ++ " - " ++ show cs

generatePaths :: [Cave] -> [Cave] -> Cave -> [[Cave]]
generatePaths cs p now =
    let nextDestinations = filter ((\n -> n `elem` getConnectionCave now && not (all isLower n && n `elem` fmap getNameCave p)) . getNameCave) cs
     in if null nextDestinations || isEnd now
            then [p ++ [now] | isEnd now]
            else concatMap (generatePaths cs (p ++ [now])) nextDestinations

solution :: ([Cave] -> [Cave] -> Cave -> [[Cave]]) -> String -> Int
solution g s =
    let cs = parseInput s
        start = head $ filter isStart cs
     in length $ g cs [] start

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution generatePaths <$> input

isASmallCaveVisitedTwiceInPath :: [Cave] -> Bool
isASmallCaveVisitedTwiceInPath =
    any ((> 1) . length) . group . sort . fmap getNameCave . filter isSmall

generatePaths' :: [Cave] -> [Cave] -> Cave -> [[Cave]]
generatePaths' cs p now =
    let nextDestinations =
            filter
                ( ( \n ->
                        n `elem` getConnectionCave now
                            && n /= "start"
                            && not (all isLower n && n `elem` fmap getNameCave p && isASmallCaveVisitedTwiceInPath (p ++ [now]))
                  )
                    . getNameCave
                )
                cs
     in if null nextDestinations || isEnd now
            then [p ++ [now] | isEnd now]
            else concatMap (generatePaths' cs (p ++ [now])) nextDestinations

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = solution generatePaths' <$> input

input :: IO String
input = readFile "input/2021/12December.txt"

inputTestS :: String
inputTestS =
    "start-A\n\
    \start-b\n\
    \A-c\n\
    \A-b\n\
    \b-d\n\
    \A-end\n\
    \b-end"

inputTestM :: String
inputTestM =
    "dc-end\n\
    \HN-start\n\
    \start-kj\n\
    \dc-start\n\
    \dc-HN\n\
    \LN-dc\n\
    \HN-end\n\
    \kj-sa\n\
    \kj-HN\n\
    \kj-dc"

inputTestL :: String
inputTestL =
    "fs-end\n\
    \he-DX\n\
    \fs-he\n\
    \start-DX\n\
    \pj-DX\n\
    \end-zg\n\
    \zg-sl\n\
    \zg-pj\n\
    \pj-he\n\
    \RW-he\n\
    \fs-DX\n\
    \pj-RW\n\
    \zg-RW\n\
    \start-pj\n\
    \he-WI\n\
    \zg-he\n\
    \pj-fs\n\
    \start-RW"
