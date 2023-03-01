module TwentyTwentyTwo.TwentySecondDecember where

import Data.Bifunctor (bimap, first, second)
import Data.Char (isDigit)
import Data.List (find, groupBy)
import Data.Map (Map, alter, empty, fromList, keys, toList, (!))
import qualified Data.Map as M (filter, lookup, member)
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
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
    | val == Nothing -- (wrapAround pos dir fieldMap)
        =
        moveSteps fieldMap wrapFunction prevPos (wrapFunction pos dir) dir n
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

data Face
    = EmptyFace
        { top :: [Position]
        , bottom :: [Position]
        , left :: [Position]
        , right :: [Position]
        , fid :: Int
        , faceTopId :: Int
        , faceBottomId :: Int
        , faceLeftId :: Int
        , faceRightId :: Int
        }
    | Face
        { top :: [Position]
        , bottom :: [Position]
        , left :: [Position]
        , right :: [Position]
        , fid :: Int
        , faceTopId :: Int
        , faceBottomId :: Int
        , faceLeftId :: Int
        , faceRightId :: Int
        }
    deriving (Show, Ord, Eq)

data Cube = Cube
    { c_front :: Maybe Face
    , c_back :: Maybe Face
    , c_left :: Maybe Face
    , c_right :: Maybe Face
    , c_top :: Maybe Face
    , c_bottom :: Maybe Face
    }
    deriving (Show)

isEmptyFace :: Face -> Bool
isEmptyFace (EmptyFace{}) = True
isEmptyFace _ = False

test = ((\x -> foldCube x emptyCube) . (\x -> generateFacesMap x maxX maxY) . (\fm -> faceBoundaries fm faceSize) . fst) testInput
  where
    faceSize = 4
    (_, (maxX, maxY)) = (bounds . fst) testInput

test2 = do
    i <- input
    let faceSize = 50
        (_, (maxX, maxY)) = (bounds . fst) i
    return $ ((\x -> foldCube x emptyCube) . (\x -> generateFacesMap x maxX maxY) . (\fm -> faceBoundaries fm faceSize) . fst) i

faceBoundaries :: Map Position Field -> Int -> [Face]
faceBoundaries fieldMap faceSize = buildFace edges 0
  where
    ((minX, minY), (maxX, maxY)) = bounds fieldMap
    edges = [(x, y) | x <- [minX, minX + faceSize .. maxX], y <- [minY, minY + faceSize .. maxY]]
    buildFace [] fid = []
    buildFace (e : es) fid =
        let brb = bimap (+ (faceSize - 1)) (+ (faceSize - 1)) e
            check = M.lookup brb fieldMap >> M.lookup e fieldMap
            face =
                maybe
                    ( EmptyFace
                        { top = fmap (\x -> (x, snd e)) [fst e .. fst brb]
                        , bottom = fmap (\x -> (x, snd brb)) [fst e .. fst brb]
                        , left = fmap (\y -> (fst e, y)) [snd e .. snd brb]
                        , right = fmap (\y -> (fst brb, y)) [snd e .. snd brb]
                        , fid = fid
                        , faceTopId = -1
                        , faceBottomId = -1
                        , faceLeftId = -1
                        , faceRightId = -1
                        }
                    )
                    ( const
                        Face
                            { top = fmap (\x -> (x, snd e)) [fst e .. fst brb]
                            , bottom = fmap (\x -> (x, snd brb)) [fst e .. fst brb]
                            , left = fmap (\y -> (fst e, y)) [snd e .. snd brb]
                            , right = fmap (\y -> (fst brb, y)) [snd e .. snd brb]
                            , fid = fid
                            , faceTopId = -1
                            , faceBottomId = -1
                            , faceLeftId = -1
                            , faceRightId = -1
                            }
                    )
                    check
         in if fst brb > maxX || snd brb > maxY then buildFace es fid else face : buildFace es (fid + 1)

-- Check if two faces share an edge
shareEdge :: Face -> Face -> Int -> Int -> Maybe (Face, Face)
shareEdge f1 f2 maxX maxY
    | ((fmap (second (\x -> (x - 1) `mod` (maxY + 1))) . top) f1) == (bottom f2) = Just (f1{faceTopId = fid f2}, f2{faceBottomId = fid f1})
    | ((fmap (second ((`mod` (maxY + 1)) . (+ 1))) . bottom) f1) == (top f2) = Just (f1{faceBottomId = fid f2}, f2{faceTopId = fid f1})
    | ((fmap (first (\x -> (x - 1) `mod` (maxX + 1))) . left) f1) == (right f2) = Just (f1{faceLeftId = fid f2}, f2{faceRightId = fid f1})
    | ((fmap (first ((`mod` (maxX + 1)) . (+ 1))) . right) f1) == (left f2) = Just (f1{faceRightId = fid f2}, f2{faceLeftId = fid f1})
    | otherwise = Nothing

mergeFaces :: Face -> Face -> Face
mergeFaces f1 f2
    | fid f1 == fid f2 =
        f1
            { faceTopId = max (faceTopId f1) (faceTopId f2)
            , faceBottomId = max (faceBottomId f1) (faceBottomId f2)
            , faceLeftId = max (faceLeftId f1) (faceLeftId f2)
            , faceRightId = max (faceRightId f1) (faceRightId f2)
            }
    | otherwise = error "can't merge 2 different faces"

generateFacesMap :: [Face] -> Int -> Int -> Map Int Face
generateFacesMap faces maxX maxY =
    let mergeF f1 (Just f) = Just (mergeFaces f f1)
        mergeF f1 Nothing = Just f1
        connections =
            ( M.filter (not . isEmptyFace)
                . foldl (\acc (f1, f2) -> alter (mergeF f2) (fid f2) (alter (mergeF f1) (fid f1) acc)) empty
                . catMaybes
            )
                [shareEdge f1' f2' maxX maxY | f1' <- faces, f2' <- faces, f1' /= f2']
     in connections

emptyCube :: Cube
emptyCube = Cube{c_front = Nothing, c_back = Nothing, c_left = Nothing, c_right = Nothing, c_bottom = Nothing, c_top = Nothing}
getCubeFaces :: Cube -> [Face]
getCubeFaces c = mapMaybe (\f -> f c) [c_front, c_back, c_left, c_right, c_bottom, c_top]
getCubeFaces' :: Cube -> [Cube -> Maybe Face] -> [Face]
getCubeFaces' c sel = mapMaybe (\f -> f c) sel

foldCube :: Map Int Face -> Cube -> Cube
foldCube fm c = if ((== 6) . length . getCubeFaces) c' then c' else foldCube fm c'
  where
    c' = traceShowId $ foldl addFace c fm

addFace :: Cube -> Face -> Cube
addFace c f
    | (null . getCubeFaces) c = c{c_front = Just f}
    | (fid f) `elem` (fmap fid (getCubeFaces c)) = c
    | (fid f) `elem` fmap faceTopId (getCubeFaces' c [c_front, c_left, c_right, c_back]) || lastFaceCondition c_top = c{c_top = Just f{faceTopId = maybe (faceTopId f) fid (c_back c), faceBottomId = maybe (faceBottomId f) fid (c_front c), faceRightId = maybe (faceRightId f) fid (c_right c), faceLeftId = maybe (faceLeftId f) fid (c_left c)}}
    | (fid f) `elem` fmap faceBottomId (getCubeFaces' c [c_front, c_left, c_right, c_back]) || lastFaceCondition c_bottom = c{c_bottom = Just f{faceTopId = maybe (faceTopId f) fid (c_back c), faceBottomId = maybe (faceBottomId f) fid (c_front c), faceRightId = maybe (faceRightId f) fid (c_right c), faceLeftId = maybe (faceLeftId f) fid (c_left c)}}
    | (fid f) `elem` fmap faceRightId (getCubeFaces' c [c_front, c_top, c_bottom]) || lastFaceCondition c_right = c{c_right = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_back c), faceLeftId = maybe (faceLeftId f) fid (c_front c)}}
    | (fid f) `elem` fmap faceLeftId (getCubeFaces' c [c_back]) = c{c_right = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_back c), faceLeftId = maybe (faceLeftId f) fid (c_front c)}}
    | (fid f) `elem` fmap faceLeftId (getCubeFaces' c [c_front, c_top, c_bottom]) || lastFaceCondition c_left = c{c_left = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_front c), faceLeftId = maybe (faceLeftId f) fid (c_back c)}}
    | (fid f) `elem` fmap faceRightId (getCubeFaces' c [c_back]) = c{c_left = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_front c), faceLeftId = maybe (faceLeftId f) fid (c_back c)}}
    | (fid f) `elem` fmap faceTopId (getCubeFaces' c [c_top, c_bottom]) || lastFaceCondition c_back = c{c_back = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_left c), faceLeftId = maybe (faceLeftId f) fid (c_right c)}}
    | (fid f) `elem` fmap faceLeftId (getCubeFaces' c [c_left]) = c{c_back = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_left c), faceLeftId = maybe (faceLeftId f) fid (c_right c)}}
    | (fid f) `elem` fmap faceRightId (getCubeFaces' c [c_right]) = c{c_back = Just f{faceTopId = maybe (faceTopId f) fid (c_top c), faceBottomId = maybe (faceBottomId f) fid (c_bottom c), faceRightId = maybe (faceRightId f) fid (c_left c), faceLeftId = maybe (faceLeftId f) fid (c_right c)}}
    | otherwise = c
  where
    lastFaceCondition sel = ((== 5) . length . getCubeFaces) c && (not . isJust . sel) c

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 =
    ( \(mf, ms) ->
        let faceSize = 50
            (_, (maxX, maxY)) = bounds mf
            cube = ((\x -> foldCube x emptyCube) . (\x -> generateFacesMap x maxX maxY) . (\fm -> faceBoundaries fm faceSize)) mf
         in solution (mf, ms) (wrapAroundCube mf cube)
    )
        <$> input

wrapAroundCube :: Map Position Field -> Cube -> Position -> Position -> Position
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
