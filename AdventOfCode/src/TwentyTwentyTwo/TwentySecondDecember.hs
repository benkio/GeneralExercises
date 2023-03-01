module TwentyTwentyTwo.TwentySecondDecember where

import Data.Char (isDigit)
import Data.List (find, groupBy)
import Data.Map (Map, fromList, keys, toList, (!))
import qualified Data.Map as M (filter, lookup, member)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Debug.Trace

data Field = Empty | Wall deriving (Eq, Show)
type Position = (Int, Int)
data Move = L | R | Move Int deriving (Eq, Show)

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

applyMoves :: Map Position Field -> [Move] -> (Position, Position)
applyMoves fieldMap moves =
    let startPos = findStartPos fieldMap
     in applyMovesHelper fieldMap moves startPos (1, 0)

findStartPos :: Map Position Field -> Position
findStartPos fieldMap =
    let ((minX, _), (maxX, _)) = bounds fieldMap
     in fromJust $ find (isJust . (`M.lookup` fieldMap)) [(x, 0) | x <- [minX .. maxX]]

applyMovesHelper :: Map Position Field -> [Move] -> Position -> Position -> (Position, Position)
applyMovesHelper fieldMap [] curPos dir = (curPos, dir)
applyMovesHelper fieldMap (move : rest) curPos dir =
    case move of
        L -> applyMovesHelper fieldMap rest curPos (rotateLeft dir)
        R -> applyMovesHelper fieldMap rest curPos (rotateRight dir)
        Move n ->
            let newPos = trace ("curPos: " ++ (show curPos) ++ " dir: " ++ (show dir) ++ " n: " ++ (show n)) $ moveSteps fieldMap curPos curPos dir n
             in applyMovesHelper fieldMap rest newPos dir
  where
    rotateLeft :: Position -> Position
    rotateLeft (x, y) = (-y, x)

    rotateRight :: Position -> Position
    rotateRight (x, y) = (y, -x)

moveSteps :: Map Position Field -> Position -> Position -> Position -> Int -> Position
moveSteps _ _ pos _ 0 = pos
moveSteps fieldMap prevPos pos dir n
    | val == Just Wall = prevPos
    | val == Nothing = moveSteps fieldMap prevPos (wrapAround pos dir fieldMap) dir n
    | otherwise = moveSteps fieldMap pos (addPos pos dir) dir (n - 1)
  where
    val = M.lookup pos fieldMap
    addPos :: Position -> Position -> Position
    addPos (x1, y1) (x2, y2) = (x1 + x2, y1 - y2)

wrapAround :: Position -> Position -> Map Position Field -> Position
wrapAround (_, y) (1, 0) fm = (fst (boundsByRow y fm), y)
wrapAround (_, y) (-1, 0) fm = (snd (boundsByRow y fm), y)
wrapAround (x, _) (0, 1) fm = (x, snd (boundsByColumn x fm))
wrapAround (x, _) (0, -1) fm = (x, fst (boundsByColumn x fm))

bounds :: Map Position Field -> ((Int, Int), (Int, Int))
bounds fieldMap =
    let xs = map fst $ keys fieldMap
        ys = map snd $ keys fieldMap
     in ((minimum xs, minimum ys), (maximum xs, maximum ys))

boundsByRow :: Int -> Map Position Field -> (Int, Int)
boundsByRow row fm = boundsBy (\(x, y) -> y == row) fst fm

boundsByColumn :: Int -> Map Position Field -> (Int, Int)
boundsByColumn column fm = boundsBy (\(x, y) -> x == column) snd fm

boundsBy :: (Position -> Bool) -> (Position -> Int) -> Map Position Field -> (Int, Int)
boundsBy fil sel fieldMap =
    let rowPositions = filter fil $ keys fieldMap
        minPos = minimum rowPositions
        maxPos = maximum rowPositions
     in (sel minPos, sel maxPos)

solution :: (Map Position Field, [Move]) -> Int
solution = (uncurry calculatePassword . uncurry applyMoves)
  where
    calculatePassword (x, y) dir = (y + 1) * 1000 + (x + 1) * 4 + directionValue dir
    directionValue (1, 0) = 0
    directionValue (-1, 0) = 2
    directionValue (0, 1) = 3
    directionValue (0, -1) = 1

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = undefined

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = undefined

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
     in fromList $ mapMaybe (\(x, m) -> fmap (\y -> (x, y)) m) cells

parseMoves :: String -> [Move]
parseMoves [] = []
parseMoves (x : xs) =
    case x of
        'L' -> L : parseMoves xs
        'R' -> R : parseMoves xs
        _ ->
            let (numStr, rest) = span isDigit (x : xs)
                num = read numStr
             in Move num : parseMoves rest

parseCell :: Char -> Maybe Field
parseCell '.' = Just Empty
parseCell '#' = Just Wall
parseCell ' ' = Nothing
parseCell _ = error "Invalid character in input"

reconstructInput :: Map Position Field -> String
reconstructInput fieldMap =
    let maxX = maximum $ map fst $ keys fieldMap
        maxY = maximum $ map snd $ keys fieldMap
        charForPosition pos@(x, y) =
            if M.member pos fieldMap
                then case fieldMap ! pos of
                    Empty -> '.'
                    Wall -> '#'
                else ' '
        lines' = [[charForPosition (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
     in unlines lines'
