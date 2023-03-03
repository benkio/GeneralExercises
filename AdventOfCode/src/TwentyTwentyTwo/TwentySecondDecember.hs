module TwentyTwentyTwo.TwentySecondDecember where

import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (elemIndex, find, groupBy, nub, sort, (\\))
import Data.Map (Map, alter, empty, fromList, keys, toList, (!))
import qualified Data.Map as M (filter, lookup, member, notMember)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, listToMaybe, mapMaybe)
import Debug.Trace
import Text.Printf

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

applyMoves :: Map Position Field -> (Position -> Position -> (Position, Position)) -> [Move] -> (Position, Position)
applyMoves fieldMap wrapFunc moves =
    let startPos = findStartPos fieldMap
     in applyMovesHelper fieldMap wrapFunc moves startPos (1, 0)

findStartPos :: Map Position Field -> Position
findStartPos fieldMap =
    let ((minX, _), (maxX, _)) = bounds fieldMap
     in fromJust $ find (isJust . (`M.lookup` fieldMap)) [(x, 0) | x <- [minX .. maxX]]

applyMovesHelper :: Map Position Field -> (Position -> Position -> (Position, Position)) -> [Move] -> Position -> Position -> (Position, Position)
applyMovesHelper fieldMap wrapFunc [] curPos dir = (curPos, dir)
applyMovesHelper fieldMap wrapFunc (move : rest) curPos dir =
    case move of
        L -> applyMovesHelper fieldMap wrapFunc rest curPos (rotateLeft dir)
        R -> applyMovesHelper fieldMap wrapFunc rest curPos (rotateRight dir)
        Move n ->
            let (newPos, newDir) = moveSteps fieldMap wrapFunc (curPos, dir) curPos dir n
             in applyMovesHelper fieldMap wrapFunc rest newPos newDir
  where
    rotateLeft :: Position -> Position
    rotateLeft (x, y) = (-y, x)

    rotateRight :: Position -> Position
    rotateRight (x, y) = (y, -x)

moveSteps :: Map Position Field -> (Position -> Position -> (Position, Position)) -> (Position, Position) -> Position -> Position -> Int -> (Position, Position)
moveSteps fieldMap wrapFunction (prevPos, prevDir) pos dir n
    | val == Just Wall = (prevPos, prevDir)
    | val == Nothing = moveSteps fieldMap wrapFunction (prevPos, prevDir) posWrapped newDir n
    | n == 0 = (pos, dir)
    | otherwise = moveSteps fieldMap wrapFunction (pos, dir) (addPos pos dir) dir (n - 1)
  where
    val = M.lookup pos fieldMap
    (posWrapped, newDir) = (wrapFunction pos dir)
    addPos :: Position -> Position -> Position
    addPos (x1, y1) (x2, y2) = (x1 + x2, y1 - y2)

wrapAround :: Map Position Field -> Position -> Position -> (Position, Position)
wrapAround fm (_, y) dir@(1, 0) = ((fst (boundsByRow y fm), y), dir)
wrapAround fm (_, y) dir@(-1, 0) = ((snd (boundsByRow y fm), y), dir)
wrapAround fm (x, _) dir@(0, 1) = ((x, snd (boundsByColumn x fm)), dir)
wrapAround fm (x, _) dir@(0, -1) = ((x, fst (boundsByColumn x fm)), dir)

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

solution :: (Map Position Field, [Move]) -> (Position -> Position -> (Position, Position)) -> Int
solution (mf, ms) wrapFunc = uncurry calculatePassword $ applyMoves mf wrapFunc ms
  where
    calculatePassword (x, y) dir = (y + 1) * 1000 + (x + 1) * 4 + directionValue dir
    directionValue (1, 0) = 0
    directionValue (-1, 0) = 2
    directionValue (0, 1) = 3
    directionValue (0, -1) = 1

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = (\(mf, ms) -> solution (mf, ms) (wrapAround mf)) <$> input

data Edge = E {e1 :: Position, e2 :: Position}

instance Eq Edge where
    (==) e1 e2 = (sort . edgeToPosition) e1 == (sort . edgeToPosition) e2
instance Show Edge where
    show (E{e1, e2}) = printf "E %s - %s" (show e1) (show e2)

data Cube = Cube
    { edges :: [Edge]
    , zips :: [([Position], [Position])]
    }
    deriving (Show)

emptyCube :: Cube
emptyCube = Cube{edges = [], zips = []}

edgeToPosition :: Edge -> [Position]
edgeToPosition (E{e1, e2}) = [e1, e2]

findByNeighboors :: Int -> Map Position Field -> Position -> Maybe Position
findByNeighboors near mf (x, y) =
    if length (neighboors mf (x, y)) == near
        then Just (x, y)
        else Nothing

isAngleEdge = findByNeighboors 8 -- edge that needs 1 zips to complete
isFlatEdge = findByNeighboors 6 -- edge that needs 2 zips to complete
isFloatingEdge = findByNeighboors 4 -- edge that needs 3 zips to complete
isEdge mf p = (any isJust . fmap (\f -> f mf p)) [isFloatingEdge, isFlatEdge, isAngleEdge, findByNeighboors 7]

perimeter mf = (filter (isEdge mf) . keys) mf
cubeZips :: Cube -> [Position]
cubeZips = nub . concat . uncurry (++) . unzip . zips

neighboors mf (x, y) = filter (`M.member` mf) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]
missingNeighboors mf (x, y) = filter (`M.notMember` mf) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]

searchEdges :: Map Position Field -> [Edge]
searchEdges mf = (fmap buildEdge . mapMaybe (isAngleEdge mf) . keys) mf
  where
    buildEdge :: Position -> Edge
    buildEdge (x, y) =
        let (nx, ny) = (bimap (\a -> a - x) (\b -> b - y) . head) $ missingNeighboors mf (x, y)
         in E{e1 = (x, y + ny), e2 = (x + nx, y)}

buildCube :: Map Position Field -> Int -> Cube -> [Edge] -> Cube
buildCube mf faceSize c [] = c
buildCube mf faceSize c (e : es) =
    maybe (buildCube mf faceSize c es) loop mayEdge
  where
    mayEdge =
        -- trace (printf "debug: %s - %s" (show e) (show (edges c))) $ traceShowId $
        (zipFromEdge mf faceSize (edges c) e)
    buildCube' zs = c{edges = edges c ++ [e], zips = zips c ++ [zs]}
    recurseIf e' c' =
        if e' `notElem` edges c
            then buildCube mf faceSize c' (es ++ [e'])
            else buildCube mf faceSize c' es
    loop (e', zs) = recurseIf e' $ buildCube' zs

zipFromEdge :: Map Position Field -> Int -> [Edge] -> Edge -> Maybe (Edge, ([Position], [Position]))
zipFromEdge mf faceSize es e@(E{e1 = p, e2 = p'}) =
    do
        (inner1, mayOuter1) <- calculateEdges mf faceSize es p
        (inner2, mayOuter2) <- calculateEdges mf faceSize es p'
        let nextEdge = E{e1 = fromMaybe inner1 mayOuter1, e2 = fromMaybe inner2 mayOuter2}
            zipEdges a b = [(x, y) | x <- [min (fst a) (fst b) .. max (fst a) (fst b)], y <- [min (snd a) (snd b) .. max (snd a) (snd b)]]
            orderZip end zs = if last zs == end then zs else reverse zs
        return (nextEdge, ((orderZip p . zipEdges p) inner1, (orderZip p' . zipEdges p') inner2))

calculateEdges :: Map Position Field -> Int -> [Edge] -> Position -> Maybe (Position, Maybe Position)
calculateEdges mf faceSize es e1 =
    (\e -> (e, edges faceSize)) <$> edges (faceSize - 1)
  where
    diagonalMissingNeighboors = (filter (\(x, y) -> x /= fst e1 && y /= snd e1) . missingNeighboors mf) e1
    computeEdgesFromDiagonalNeighboors step (x, y) =
        let offsetX = ((x - (fst e1)) * step)
            offsetY = ((y - (snd e1)) * step)
         in [((fst e1, snd e1 + offsetY)), ((fst e1 + offsetX, snd e1))]
    cleanUnwantedEdges filterEdgeF = (listToMaybe . filter filterEdgeF)
    filterEdgeF p =
        p `notElem` (concatMap (\(x, y) -> [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]) . concatMap edgeToPosition) es
            && isEdge mf p
    edges step = cleanUnwantedEdges filterEdgeF $ computeEdgesFromDiagonalNeighboors step =<< diagonalMissingNeighboors

test = (buildCube (fst testInput) 4 emptyCube . searchEdges . fst) testInput
test2 = ((\mf -> calculateEdges mf 50 [] (49, 150)) . fst) <$> input
test1 = do
    (mf, ms) <- input
    let edges = searchEdges mf
        cube = buildCube mf 50 emptyCube edges
        ps = perimeter mf
    return $ ps \\ (cubeZips cube)
test3 = ((perimeter (fst testInput) \\) . cubeZips . buildCube (fst testInput) 4 emptyCube . searchEdges . fst) testInput

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = solution2 50 <$> input

solution2 :: Int -> (Map Position Field, [Move]) -> Int
solution2 faceSize (mf, ms) =
    let edges = searchEdges mf
        cube = buildCube mf faceSize emptyCube edges
     in solution (mf, ms) (wrapAroundCube mf cube)

wrapAroundCube :: Map Position Field -> Cube -> Position -> Position -> (Position, Position)
wrapAroundCube mf cube pos dir =
    (jumpToCubeFace, newDir)
  where
    prevPos = (fst pos - fst dir, snd pos + snd dir)
    jumpToCubeFace' = mapMaybe selectElem (zips cube)
    jumpToCubeFace = if null jumpToCubeFace' then error (printf "pos: %s - prevPos: %s - dir: %s" (show pos) (show prevPos) (show dir)) else head jumpToCubeFace'
    newDir = ortogonalDirection mf jumpToCubeFace
    selectElem (xs, ys)
        | prevPos `elem` xs = (ys !!) <$> elemIndex prevPos xs
        | prevPos `elem` ys = (xs !!) <$> elemIndex prevPos ys
        | otherwise = Nothing

ortogonalDirection :: Map Position Field -> Position -> Position
ortogonalDirection mf (x, y)
    | M.lookup (x + 1, y) mf == Nothing = (-1, 0)
    | M.lookup (x - 1, y) mf == Nothing = (1, 0)
    | M.lookup (x, y + 1) mf == Nothing = (0, 1)
    | M.lookup (x, y - 1) mf == Nothing = (0, -1)
    | otherwise = error $ "no blank space near " ++ show (x, y)

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
