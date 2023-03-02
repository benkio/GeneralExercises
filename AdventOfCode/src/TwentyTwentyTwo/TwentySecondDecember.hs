module TwentyTwentyTwo.TwentySecondDecember where

import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (find, groupBy, nub, sort, (\\))
import Data.Map (Map, alter, empty, fromList, keys, toList, (!))
import qualified Data.Map as M (filter, lookup, member, notMember)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
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

applyMoves :: Map Position Field -> (Position -> Position -> Position) -> [Move] -> (Position, Position)
applyMoves fieldMap wrapFunc moves =
    let startPos = findStartPos fieldMap
     in applyMovesHelper fieldMap wrapFunc moves startPos (1, 0)

findStartPos :: Map Position Field -> Position
findStartPos fieldMap =
    let ((minX, _), (maxX, _)) = bounds fieldMap
     in fromJust $ find (isJust . (`M.lookup` fieldMap)) [(x, 0) | x <- [minX .. maxX]]

applyMovesHelper :: Map Position Field -> (Position -> Position -> Position) -> [Move] -> Position -> Position -> (Position, Position)
applyMovesHelper fieldMap wrapFunc [] curPos dir = (curPos, dir)
applyMovesHelper fieldMap wrapFunc (move : rest) curPos dir =
    case move of
        L -> applyMovesHelper fieldMap wrapFunc rest curPos (rotateLeft dir)
        R -> applyMovesHelper fieldMap wrapFunc rest curPos (rotateRight dir)
        Move n ->
            let newPos = moveSteps fieldMap wrapFunc curPos curPos dir n
             in applyMovesHelper fieldMap wrapFunc rest newPos dir
  where
    rotateLeft :: Position -> Position
    rotateLeft (x, y) = (-y, x)

    rotateRight :: Position -> Position
    rotateRight (x, y) = (y, -x)

moveSteps :: Map Position Field -> (Position -> Position -> Position) -> Position -> Position -> Position -> Int -> Position
moveSteps fieldMap wrapFunction prevPos pos dir n
    | val == Just Wall = prevPos
    | val == Nothing = moveSteps fieldMap wrapFunction prevPos (wrapFunction pos dir) dir n
    | n == 0 = pos
    | otherwise = moveSteps fieldMap wrapFunction pos (addPos pos dir) dir (n - 1)
  where
    val = M.lookup pos fieldMap
    addPos :: Position -> Position -> Position
    addPos (x1, y1) (x2, y2) = (x1 + x2, y1 - y2)

wrapAround :: Map Position Field -> Position -> Position -> Position
wrapAround fm (_, y) (1, 0) = (fst (boundsByRow y fm), y)
wrapAround fm (_, y) (-1, 0) = (snd (boundsByRow y fm), y)
wrapAround fm (x, _) (0, 1) = (x, snd (boundsByColumn x fm))
wrapAround fm (x, _) (0, -1) = (x, fst (boundsByColumn x fm))

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

solution :: (Map Position Field, [Move]) -> (Position -> Position -> Position) -> Int
solution (mf, ms) wrapFunc = uncurry calculatePassword $ applyMoves mf wrapFunc ms
  where
    calculatePassword (x, y) dir = (y + 1) * 1000 + (x + 1) * 4 + directionValue dir
    directionValue (1, 0) = 0
    directionValue (-1, 0) = 2
    directionValue (0, 1) = 3
    directionValue (0, -1) = 1

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = (\(mf, ms) -> solution (mf, ms) (wrapAround mf)) <$> input

data Edge = E1 {e1 :: Position} | E2 {e1 :: Position, e2 :: Position} deriving (Show)

instance Eq Edge where
    (==) e1 e2 = (sort . edgeToPosition) e1 == (sort . edgeToPosition) e2

data Cube = Cube
    { edges :: [Edge]
    , zips :: [([Position], [Position])]
    }
    deriving (Show)

emptyCube :: Cube
emptyCube = Cube{edges = [], zips = []}

edgeToPosition :: Edge -> [Position]
edgeToPosition (E1{e1}) = [e1]
edgeToPosition (E2{e1, e2}) = [e1, e2]

findEdge :: Int -> Map Position Field -> Position -> Maybe Edge
findEdge near mf (x, y) =
    if length (neighboors mf (x, y)) == near
        then Just $ E1{e1 = (x, y)}
        else Nothing

isAngleEdge = findEdge 8 -- edge that needs 1 zips to complete
isFlatEdge = findEdge 6 -- edge that needs 2 zips to complete
isFloatingEdge = findEdge 4 -- edge that needs 3 zips to complete
isEdge mf p = (any isJust . fmap (\f -> f mf p)) [isFloatingEdge, isFlatEdge, isAngleEdge, findEdge 7]

perimeter mf = (filter (isEdge mf) . keys) mf
cubeZips :: Cube -> [Position]
cubeZips = nub . concat . uncurry (++) . unzip . zips

neighboors mf (x, y) = filter (`M.member` mf) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]
missingNeighboors mf (x, y) = filter (`M.notMember` mf) [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]

searchEdges :: Map Position Field -> [Edge]
searchEdges mf = (mapMaybe (isAngleEdge mf) . keys) mf

buildCube :: Map Position Field -> Int -> Cube -> [Edge] -> Cube
buildCube mf faceSize c [] = c
buildCube mf faceSize c (e : es) =
    maybe
        (buildCube mf faceSize c es)
        ( \(e', zs) ->
            let cube' =
                    c
                        { edges = edges c ++ [e]
                        , zips = zips c ++ [zs]
                        }
             in if e' `notElem` edges cube'
                    then buildCube mf faceSize cube' (es ++ [e'])
                    else buildCube mf faceSize cube' es
        )
        (zipFromEdge mf faceSize (edges c) e)

zipFromEdge :: Map Position Field -> Int -> [Edge] -> Edge -> Maybe (Edge, ([Position], [Position]))
zipFromEdge mf faceSize es e@(E1{e1 = p}) = do
    nemaybe <- neighboorEdges mf faceSize es e
    let [ne1, ne2] = trace (printf "e: %s - nemaybe: %s" (show e) (show nemaybe)) $ nemaybe
        zipEdges p' = [(x, y) | x <- [min (fst p) (fst p') .. max (fst p) (fst p')], y <- [min (snd p) (snd p') .. max (snd p) (snd p')]]
        orderZip zs = if last zs == p then zs else reverse zs
    return (E2{e1 = ne1, e2 = ne2}, ((orderZip . zipEdges) ne1, (orderZip . zipEdges) ne2))
zipFromEdge mf faceSize es e@(E2{e1 = p, e2 = p'}) = do
    nemaybe <- neighboorEdges mf faceSize es e
    let [ne1, ne2] = nemaybe
        zipEdges a b = [(x, y) | x <- [min (fst a) (fst b) .. max (fst a) (fst b)], y <- [min (snd a) (snd b) .. max (snd a) (snd b)]]
        orderZip end zs = if last zs == end then zs else reverse zs
    return
        ( E2{e1 = ne1, e2 = ne2}
        , ((orderZip p . zipEdges p) ne1, (orderZip p' . zipEdges p') ne2)
        )

neighboorEdges :: Map Position Field -> Int -> [Edge] -> Edge -> Maybe [Position]
neighboorEdges mf faceSize es (E1{e1}) =
    ( -- (\xs -> if length xs == 2 then Just xs else Nothing)
      Just
        . filter (\p -> p `notElem` (concatMap (\(x, y) -> [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]) . concatMap edgeToPosition) es && M.member p mf)
        . concatMap
            ( \(x, y) ->
                let offsetX = fst e1 + ((x - (fst e1)) * faceSize)
                    offsetY = snd e1 + ((y - (snd e1)) * faceSize)
                 in [(fst e1, offsetY), (offsetX, snd e1)]
            )
        . filter (\(x, y) -> x /= fst e1 && y /= snd e1)
        . missingNeighboors mf
    )
        e1
neighboorEdges mf faceSize es (E2{e1, e2}) = do
    ps1' <- neighboorEdges mf faceSize es (E1{e1 = e1})
    ps2' <- neighboorEdges mf faceSize es (E1{e1 = e2})
    ps1 <- if null ps1' then neighboorEdges mf (faceSize - 1) es (E1{e1 = e1}) else Just ps1'
    ps2 <- if null ps2' then neighboorEdges mf (faceSize - 1) es (E1{e1 = e2}) else Just ps2'
    if (length . nub) (ps1 ++ ps2) /= 2
        then Nothing -- error ("erroooor: " ++ (show (ps1 ++ ps2)) ++ " e1 " ++ show e1 ++ " e2 " ++ show e2)
        else Just $ nub $ ps1 ++ ps2

test = (buildCube (fst testInput) 4 emptyCube . searchEdges . fst) testInput
test5 = (buildCube (fst testInput) 50 emptyCube . searchEdges . fst) <$> input
test4 = ((\\ perimeter (fst testInput)) . cubeZips . buildCube (fst testInput) 4 emptyCube . searchEdges . fst) testInput
test2 = neighboorEdges (fst testInput) 3 [] (E1{e1 = (15, 8)})
test3 = buildCube (fst testInput) 4 emptyCube [E2{e1 = (11, 4), e2 = (15, 8)}]

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = undefined

--wrapAroundCube :: Map Position Field -> Cube -> Position -> Position -> Position
wrapAroundCube mf cube pos dir = undefined

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
