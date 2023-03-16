{-# LANGUAGE TupleSections #-}
module TwentyTwentyTwo.TwentyThirdDecember where

import Data.List (find, (\\))
import Data.Map (Map, adjust, elems, empty, fromList, keys, member, toList, (!))
import qualified Data.Map as M (lookup)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Text.Printf

type Position = (Int, Int)
type Elf = Int
data Direction = N | S | W | E deriving (Show, Eq)

input :: IO (Map Elf Position)
input = parseInput <$> readFile "input/2022/23December.txt"

directions :: [Direction]
directions = cycle [N, S, W, E]

directionsFrom :: Direction -> [Direction]
directionsFrom d = (take 4 . dropWhile (/= d)) directions

positionByDirection :: Position -> Direction -> ([Position], Position)
positionByDirection p@(x, y) N = (northProposalPositions p, (x, y - 1))
positionByDirection p@(x, y) S = (southProposalPositions p, (x, y + 1))
positionByDirection p@(x, y) W = (westProposalPositions p, (x - 1, y))
positionByDirection p@(x, y) E = (estProposalPositions p, (x + 1, y))
positionProposal :: Position -> Direction -> [([Position], Position)]
positionProposal p d = positionByDirection p <$> directionsFrom d

northProposalPositions :: Position -> [Position]
northProposalPositions (x, y) = [(x, y - 1), (x + 1, y - 1), (x - 1, y - 1)]
southProposalPositions :: Position -> [Position]
southProposalPositions (x, y) = [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)]
estProposalPositions :: Position -> [Position]
estProposalPositions (x, y) = [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
westProposalPositions :: Position -> [Position]
westProposalPositions (x, y) = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

elfProposal :: Map Elf Position -> Direction -> Elf -> Maybe (Position, Elf)
elfProposal m d e
    | (checkAdjcents . concatMap fst) ps = Nothing
    | otherwise = (,e) . snd <$> find (checkAdjcents . fst) ps
  where
    p = m ! e
    (x, y) = (fst p, snd p)
    ps = positionProposal p d
    checkAdjcents xs = all not $ fmap (`elem` (elems m)) xs

proposePhase :: Map Elf Position -> Direction -> [(Position, Elf)]
proposePhase m d = (removeDuplicates . mapMaybe (elfProposal m d) . keys) m
  where
    removeDuplicates :: [(Position, Elf)] -> [(Position, Elf)]
    removeDuplicates [] = []
    removeDuplicates (p : ps) =
        let duplicates = filter (\((x, y), _) -> x == ((fst . fst) p) && y == ((snd . fst) p)) ps
         in if null duplicates then p : removeDuplicates ps else removeDuplicates (ps \\ duplicates)

movePhase :: Map Elf Position -> [(Position, Elf)] -> Map Elf Position
movePhase = foldl (\acc (p, e) -> adjust (const p) e acc)

elfRound :: Map Elf Position -> Direction -> Map Elf Position
elfRound m d = movePhase m proposals
  where
    proposals = proposePhase m d

elvesRoundsTillStady :: Map Elf Position -> Int
elvesRoundsTillStady m =
    (\(_, _, _, x) -> x) $
        until
            (\(prevM, currM, _, _) -> (toList prevM) == (toList currM))
            (\(_, currM, dirs, c) -> (currM, elfRound currM (head dirs), tail dirs, c + 1))
            (empty, m, directions, 0)

elvesMoveAfterXRounds :: Map Elf Position -> Int -> Map Elf Position
elvesMoveAfterXRounds m r =
    (\(_, x, _) -> x) $
        until
            (\(x, _, _) -> x == 0)
            (\(x, currM, dirs) -> (x - 1, elfRound currM (head dirs), tail dirs))
            (r, m, directions)

countEmptyTiles :: Map Elf Position -> Int
countEmptyTiles m = length allEmptyTiles
  where
    elvesPos = elems m
    (possX, possY) = unzip elvesPos
    (minX, maxX, minY, maxY) = (minimum possX, maximum possX, minimum possY, maximum possY)
    allEmptyTiles = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY], (x, y) `notElem` elvesPos]

solution m = countEmptyTiles $ elvesMoveAfterXRounds m 10

twentyThirdDecemberSolution1 :: IO Int
twentyThirdDecemberSolution1 = solution <$> input

twentyThirdDecemberSolution2 :: IO Int
twentyThirdDecemberSolution2 = elvesRoundsTillStady <$> input

parseInput :: String -> Map Elf Position
parseInput = fromList . fmap (\(i, (p, _)) -> (i, p)) . zip [0 ..] . parseLine . (`zip` [0 ..]) . lines
  where
    parseLine :: [(String, Int)] -> [(Position, ())]
    parseLine [] = []
    parseLine ((s, y) : xs) =
        fmap (\x -> ((x, y), ())) (parseRow (zip [0 ..] s)) ++ parseLine xs
    parseRow :: [(Int, Char)] -> [Int]
    parseRow [] = []
    parseRow ((x, '.') : cs) = parseRow cs
    parseRow ((x, '#') : cs) = x : parseRow cs

testInput :: Map Elf Position
testInput =
    parseInput
        "....#..\n\
        \..###.#\n\
        \#...#.#\n\
        \.#...##\n\
        \#.###..\n\
        \##.#.##\n\
        \.#..#.."

testInput2 :: Map Elf Position
testInput2 =
    parseInput
        ".....\n\
        \..##.\n\
        \..#..\n\
        \.....\n\
        \..##.\n\
        \....."
