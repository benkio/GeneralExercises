module TwentyTwentyTwo.TwentyThirdDecember where

import Data.List (find, (\\))
import Data.Map (Map, adjust, elems, fromList, keys, member, (!))
import qualified Data.Map as M (lookup)
import Data.Maybe (mapMaybe)
import Text.Printf

type Position = (Int, Int)
type Elf = Int
data Direction = N | S | W | E deriving (Show, Eq)

input :: IO (Map Elf Position)
input = parseInput <$> readFile "input/2022/23December.txt"

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

directions :: Direction -> [Direction]
directions d = (take 4 . dropWhile (== d) . cycle) [N, S, W, E]

positionByDirection :: Position -> Direction -> ([Position], Position)
positionByDirection p@(x, y) N = (northProposalPositions p, (x, y - 1))
positionByDirection p@(x, y) S = (southProposalPositions p, (x, y + 1))
positionByDirection p@(x, y) W = (westProposalPositions p, (x - 1, y))
positionByDirection p@(x, y) E = (estProposalPositions p, (x + 1, y))
positionProposal :: Position -> Direction -> [([Position], Position)]
positionProposal p d = positionByDirection p <$> directions d

northProposalPositions :: Position -> [Position]
northProposalPositions (x, y) = [(x, y - 1), (x + 1, y - 1), (x - 1, y - 1)]
southProposalPositions :: Position -> [Position]
southProposalPositions (x, y) = [(x, y + 1), (x + 1, y + 1), (x - 1, y + 1)]
estProposalPositions :: Position -> [Position]
estProposalPositions (x, y) = [(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
westProposalPositions :: Position -> [Position]
westProposalPositions (x, y) = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]

elfProposal :: Map Elf Position -> Direction -> Elf -> Maybe (Position, Elf)
elfProposal m d e = (,e) . snd <$> find checkAdjcents ps
  where
    p = m ! e
    (x, y) = (fst p, snd p)
    ps = positionProposal p d
    checkAdjcents (xs, _) = any id $ fmap (`elem` (elems m)) xs

proposePhase :: Map Elf Position -> Direction -> [(Position, Elf)]
proposePhase m d = (removeDuplicates . mapMaybe (elfProposal m d) . keys) m
  where
    removeDuplicates :: [(Position, Elf)] -> [(Position, Elf)]
    removeDuplicates [] = []
    removeDuplicates (p : ps) =
        let duplicates = filter (\((x, y), _) -> x == ((fst . fst) p) && y == ((snd . fst) p)) ps
         in if null duplicates then p : removeDuplicates ps else removeDuplicates (ps \\ duplicates)

movePhase :: Map Elf Position -> [(Position, Elf)] -> Map Elf Position
movePhase m ps = foldl (\acc (p, e) -> adjust (const p) e m) m ps

round :: Map Elf Position -> Direction -> Map Elf Position
round m d = movePhase m proposals
  where
    proposals = proposePhase m d

twentyThirdDecemberSolution1 :: IO Int
twentyThirdDecemberSolution1 = undefined

twentyThirdDecemberSolution2 :: IO Int
twentyThirdDecemberSolution2 = undefined

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
