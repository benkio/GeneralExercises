module TwentyTwentyTwo.TwentySecondDecember where

import Control.Arrow ((&&&))
import Data.Char (isDigit)
import Data.List (find)
import Data.Map (Map, fromList, keys, member, (!?))
import Data.Maybe (mapMaybe)
import Debug.Trace
import Text.Printf

data Field = Empty | Wall deriving (Eq, Show)
type Position = (Int, Int)
data Move = ML | MR | M Int deriving (Eq, Show)
data Direction = R | D | L | U deriving (Show, Enum)

directionPassword :: Direction -> Int
directionPassword R = 0
directionPassword D = 1
directionPassword L = 2
directionPassword U = 3

moveInDirection :: Position -> Direction -> Position
moveInDirection (x, y) R = (x + 1, y)
moveInDirection (x, y) L = (x - 1, y)
moveInDirection (x, y) U = (x, y - 1)
moveInDirection (x, y) D = (x, y + 1)

startingPoint :: Map Position Field -> (Position, Direction)
startingPoint mf = head $ mapMaybe (\x -> const ((x, 0), R) <$> mf !? (x, 0)) [0 ..]

applyMoves :: Map Position Field -> [Move] -> (Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMoves _ [] wrapF pos dir = (pos, dir)
applyMoves mf (m : ms) wrapF pos dir = applyMoves mf ms wrapF newPos newDir
  where
    (newPos, newDir) =
        applyMove mf m wrapF pos dir

applyMove :: Map Position Field -> Move -> (Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMove _ ML wrapF pos R = (pos, U)
applyMove _ MR wrapF pos U = (pos, R)
applyMove _ ML wrapF pos dir = (pos, pred dir)
applyMove _ MR wrapF pos dir = (pos, succ dir)
applyMove mf (M steps) wrapF pos dir
    | steps == 0 || (checkCollision . fst) nextStep = (pos, dir)
    | otherwise = uncurry (applyMove mf (M (steps - 1)) wrapF) $ nextStep
  where
    nextStep = uncurry wrapF (moveInDirection pos dir, dir)
    checkCollision p = mf !? p == Just Wall

wrapPos :: Map Position Field -> Position -> Direction -> (Position, Direction)
wrapPos mf p R = maybe (((fst . (`rowBounds` mf) . snd) p, snd p), R) (const (p, R)) $ mf !? p
wrapPos mf p L = maybe (((snd . (`rowBounds` mf) . snd) p, snd p), L) (const (p, L)) $ mf !? p
wrapPos mf p D = maybe ((fst p, (fst . (`colBounds` mf) . fst) p), D) (const (p, D)) $ mf !? p
wrapPos mf p U = maybe ((fst p, (snd . (`colBounds` mf) . fst) p), U) (const (p, U)) $ mf !? p

rowBounds :: Int -> Map Position Field -> (Int, Int)
rowBounds row = (minimum &&& maximum) . fmap fst . filter ((== row) . snd) . keys
colBounds :: Int -> Map Position Field -> (Int, Int)
colBounds col = (minimum &&& maximum) . fmap snd . filter ((== col) . fst) . keys

calculatePassword :: Position -> Direction -> Int
calculatePassword (x, y) d = directionPassword d + 4 * (x + 1) + 1000 * (y + 1)

solution :: (Position -> Direction -> (Position, Direction)) -> Map Position Field -> [Move] -> Int
solution wrapF mf ms = uncurry calculatePassword $ applyMoves mf ms wrapF sp d
  where
    (sp, d) = startingPoint mf

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = (\(mf, ms) -> solution (wrapPos mf) mf ms) <$> input

-- 119103 too low
-- 129339 correct - final tile (83,128)
twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = undefined

input :: IO (Map Position Field, [Move])
input = parseInputWithMoves <$> readFile "input/2022/22December.txt"

testInput :: (Map Position Field, [Move])
testInput =
    parseInputWithMoves
        "        ...#\n\
        \        .#..\n\
        \        #...\n\
        \        ....\n\
        \...#.......#\n\
        \........#...\n\
        \..#....#....\n\
        \..........#.\n\
        \        ...#....\n\
        \        .....#..\n\
        \        .#......\n\
        \        ......#.\n\
        \\n\
        \10R5L5R10L4R5L5"

parseInputWithMoves :: String -> (Map Position Field, [Move])
parseInputWithMoves input =
    let (inputLines, _) = break null (lines input)
        fieldMap = parseInput (unlines inputLines)
        moves = parseMoves ((last . lines) input)
     in (fieldMap, moves)

parseInput :: String -> Map Position Field
parseInput input =
    let lines' = lines input
        indexedLines = zip [0 ..] lines'
        cells = concatMap (\(y, line) -> zipWith (\x cell -> ((x, y), parseCell cell)) [0 ..] line) indexedLines
     in fromList $ mapMaybe (\(x, m) -> fmap (x,) m) cells

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves (x : xs) =
    case x of
        'L' -> ML : parseMoves xs
        'R' -> MR : parseMoves xs
        _ ->
            let (numStr, rest) = span isDigit (x : xs)
                num = read numStr
             in M num : parseMoves rest

parseCell :: Char -> Maybe Field
parseCell '.' = Just Empty
parseCell '#' = Just Wall
parseCell ' ' = Nothing
parseCell _ = error "Invalid character in input"
