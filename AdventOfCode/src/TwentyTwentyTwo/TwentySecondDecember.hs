module TwentyTwentyTwo.TwentySecondDecember where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (cycle, delete, elemIndex, find, minimumBy, nub, nubBy, partition, (\\))
import Data.Map (Map, fromList, keys, member, notMember, (!), (!?))
import Data.Maybe (fromJust, isJust, mapMaybe, maybeToList)
import Debug.Trace
import Text.Printf

data Field = Empty | Wall deriving (Eq, Show)
type Position = (Int, Int)
data Move = ML | MR | M Int deriving (Eq, Show)
data Direction = R | D | L | U deriving (Show, Enum, Eq, Ord)

directions :: [Direction]
directions = enumFrom R

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

applyMoves :: Map Position Field -> [Move] -> (Position -> Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMoves _ [] wrapF pos dir = (pos, dir)
applyMoves mf (m : ms) wrapF pos dir = applyMoves mf ms wrapF newPos newDir
  where
    (newPos, newDir) =
        applyMove mf m wrapF pos dir

applyMove :: Map Position Field -> Move -> (Position -> Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMove _ ML wrapF pos R = (pos, U)
applyMove _ MR wrapF pos U = (pos, R)
applyMove _ ML wrapF pos dir = (pos, pred dir)
applyMove _ MR wrapF pos dir = (pos, succ dir)
applyMove mf (M steps) wrapF pos dir
    | steps == 0 || (checkCollision . fst) nextStep = (pos, dir)
    | otherwise = uncurry (applyMove mf (M (steps - 1)) wrapF) $ nextStep
  where
    nextStep = uncurry (wrapF pos) (moveInDirection pos dir, dir)
    checkCollision p = mf !? p == Just Wall

wrapPos :: Map Position Field -> Position -> Position -> Direction -> (Position, Direction)
wrapPos mf _ p R = maybe (((fst . (`rowBounds` mf) . snd) p, snd p), R) (const (p, R)) $ mf !? p
wrapPos mf _ p L = maybe (((snd . (`rowBounds` mf) . snd) p, snd p), L) (const (p, L)) $ mf !? p
wrapPos mf _ p D = maybe ((fst p, (fst . (`colBounds` mf) . fst) p), D) (const (p, D)) $ mf !? p
wrapPos mf _ p U = maybe ((fst p, (snd . (`colBounds` mf) . fst) p), U) (const (p, U)) $ mf !? p

rowBounds :: Int -> Map Position Field -> (Int, Int)
rowBounds row = (minimum &&& maximum) . fmap fst . filter ((== row) . snd) . keys
colBounds :: Int -> Map Position Field -> (Int, Int)
colBounds col = (minimum &&& maximum) . fmap snd . filter ((== col) . fst) . keys

calculatePassword :: Position -> Direction -> Int
calculatePassword (x, y) d = directionPassword d + 4 * (x + 1) + 1000 * (y + 1)

solution :: (Position -> Position -> Direction -> (Position, Direction)) -> Map Position Field -> [Move] -> Int
solution wrapF mf ms = uncurry calculatePassword $ applyMoves mf ms wrapF sp d
  where
    (sp, d) = startingPoint mf

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = (\(mf, ms) -> solution (wrapPos mf) mf ms) <$> input

data AnglePosition = TL | TR | BL | BR

data Vertex = Vertex {v1 :: Position, v2 :: Position, v3 :: Position} | IncompleteVertex {v1 :: Position, v2 :: Position} deriving (Show)
newtype Face = Face {angles :: [Position]} deriving (Show)
data Cube = Cube {faces :: [Face], vertexes :: [Vertex]} deriving (Show)
data CompleteFace = CompleteFace -- tl, tr, br, bl
    { tl :: Vertex
    , tr :: Vertex
    , br :: Vertex
    , bl :: Vertex
    }
    deriving (Show)
data CompleteCube = CompleteCube
    { front :: CompleteFace
    , back :: CompleteFace
    , left :: CompleteFace
    , right :: CompleteFace
    , top :: CompleteFace
    , bottom :: CompleteFace
    }
    deriving (Show)

instance Eq Face where
    (==) f1 f2 = all (`elem` angles f2) (angles f1) && all (`elem` angles f1) (angles f2)
instance Eq Vertex where
    (==) v1 v2 = all (`elem` vertexToPos v2) (vertexToPos v1) && all (`elem` vertexToPos v1) (vertexToPos v2)

vertexToPos (Vertex{v1 = v', v2 = v'', v3 = v'''}) = [v', v'', v''']
vertexToPos (IncompleteVertex{v1 = v', v2 = v''}) = [v', v'']
vertexIsIncomplete (IncompleteVertex{}) = True
vertexIsIncomplete _ = False
findKey :: Map Position Field -> Position -> Maybe Position
findKey mf k = mf !? k <&> const k

mergeIncompleteVertexes cvs [] = cvs
mergeIncompleteVertexes cvs (v : vs)
    | any (\cv -> all (`elem` (vertexToPos cv)) (vertexToPos v)) cvs = mergeIncompleteVertexes cvs vs
    | isJust findOverlap =
        let newVertexPositions = (fromJust . fmap (\x -> nub (vertexToPos x ++ vertexToPos v))) findOverlap
            newVertex = Vertex{v1 = newVertexPositions !! 0, v2 = newVertexPositions !! 1, v3 = newVertexPositions !! 2}
            vs' = ((\x -> delete x vs) . fromJust) findOverlap
         in newVertex : mergeIncompleteVertexes cvs vs'
    | otherwise = v : mergeIncompleteVertexes cvs vs
  where
    findOverlap = find (\x -> any (`elem` vertexToPos x) (vertexToPos v)) vs

findEdge :: Map Position Field -> Int -> Position -> Bool
findEdge mf target (x, y) = ((== target) . length) $ keyAnglePattern
  where
    keyAnglePattern = filter (`member` mf) [(a, b) | a <- [(x - 1) .. (x + 1)], b <- [(y - 1) .. (y + 1)]]

buildPartialCubes :: Map Position Field -> Int -> [Cube]
buildPartialCubes mf faceSize = (fmap toPartialCube . filter (findEdge mf 8) . keys) mf
  where
    toPartialCube (x, y)
        | (x - 1, y - 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) ((-1), (-1)), faces = [buildFace faceSize (x, y) TL, buildFace faceSize (x - 1, y) TR, buildFace faceSize (x, y - 1) BL]}
        | (x + 1, y - 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (1, (-1)), faces = [buildFace faceSize (x, y) TR, buildFace faceSize (x + 1, y) TL, buildFace faceSize (x, y - 1) BR]}
        | (x - 1, y + 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) ((-1), 1), faces = [buildFace faceSize (x, y) BL, buildFace faceSize (x - 1, y) BR, buildFace faceSize (x, y + 1) TL]}
        | (x + 1, y + 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (1, 1), faces = [buildFace faceSize (x, y) BR, buildFace faceSize (x + 1, y) BL, buildFace faceSize (x, y + 1) TR]}
    buildVertexes (x, y) (dx, dy) =
        [ Vertex{v1 = (x, y), v2 = (x + (signum dx), y), v3 = (x, y + (signum dy))}
        , IncompleteVertex{v1 = (x, y + (faceSize - 1) * (negate (signum dy))), v2 = (x + (signum dx), y + (faceSize - 1) * (negate (signum dy)))}
        , IncompleteVertex{v1 = (x + (faceSize - 1) * (negate (signum dx)), y), v2 = (x + (faceSize - 1) * (negate (signum dx)), y + (signum dy))}
        ]
            ++ checkAdjacentVertex (dx, dy) (IncompleteVertex{v1 = (x + (faceSize * signum dx), y), v2 = (x, y + (faceSize * signum dy))})
    checkAdjacentVertex :: Position -> Vertex -> [Vertex]
    checkAdjacentVertex _ v@(Vertex{}) = [v]
    checkAdjacentVertex (dx, dy) v@(IncompleteVertex{v1 = (v1x, v1y), v2 = (v2x, v2y)})
        | isJust adjectVertex1 =
            let (nvx1, nvy1) = fromJust adjectVertex1
                complete = Vertex{v1 = (v1x, v1y), v2 = (v2x, v2y), v3 = (nvx1, nvy1)}
                nextIncomplete = IncompleteVertex{v1 = (nvx1 + ((faceSize - 1) * signum dx), nvy1), v2 = (v2x + ((faceSize - 1) * negate (signum dx)), v2y)}
             in [complete, nextIncomplete]
        | isJust adjectVertex2 =
            let (nvx2, nvy2) = fromJust adjectVertex2
                complete = Vertex{v1 = (v1x, v1y), v2 = (v2x, v2y), v3 = (nvx2, nvy2)}
                nextIncomplete = IncompleteVertex{v1 = (v1x, v1y + ((faceSize - 1) * negate (signum dy))), v2 = (nvx2, nvy2 + ((faceSize - 1) * signum dy))}
             in [complete, nextIncomplete]
        | otherwise = [v]
      where
        adjectVertex1 = (mf `findKey` (v1x + dx, v1y)) <|> (mf `findKey` (v1x, v1y + dy))
        adjectVertex2 = (mf `findKey` (v2x + dx, v2y)) <|> (mf `findKey` (v2x, v2y + dy))

buildFaceAngles :: Int -> Position -> AnglePosition -> [Position]
buildFaceAngles faceSize (x, y) TL = [(x, y), (x + (faceSize - 1), y), (x + (faceSize - 1), y + (faceSize - 1)), (x, y + (faceSize - 1))]
buildFaceAngles faceSize (x, y) TR = [(x - (faceSize - 1), y), (x, y), (x, y + (faceSize - 1)), (x - (faceSize - 1), y + (faceSize - 1))]
buildFaceAngles faceSize (x, y) BR = [(x - (faceSize - 1), y - (faceSize - 1)), (x, y - (faceSize - 1)), (x, y), (x - (faceSize - 1), y)]
buildFaceAngles faceSize (x, y) BL = [(x, y - (faceSize - 1)), (x + (faceSize - 1), y - (faceSize - 1)), (x + (faceSize - 1), y), (x, y)]

buildFace :: Int -> Position -> AnglePosition -> Face
buildFace faceSize p pos = Face{angles = [tl, tr, br, bl]}
  where
    [tl, tr, br, bl] = buildFaceAngles faceSize p pos

mergeCubes :: Cube -> [Cube] -> Cube
mergeCubes c [] = c
mergeCubes cube (c : cs) = mergeCubes c' cs
  where
    mergedFaces = nub $ faces cube ++ faces c
    (incompleteVertexes, completeVertexes) = partition vertexIsIncomplete $ vertexes cube ++ vertexes c
    c' = Cube{vertexes = mergeIncompleteVertexes completeVertexes (nub incompleteVertexes), faces = mergedFaces}

findLastVertex :: Cube -> Cube
findLastVertex c =
    if ((== 7) . length) completeVertexes
        then c{vertexes = lostVertex : ((filter (not . vertexIsIncomplete) . vertexes) c)}
        else c
  where
    es = (concatMap angles . faces) c
    completeVertexes = (filter (not . vertexIsIncomplete) . vertexes) c
    vs = concatMap vertexToPos completeVertexes
    [v', v'', v'''] = es \\ vs
    lostVertex = Vertex{v1 = v', v2 = v'', v3 = v'''}

completeFace :: Cube -> Face -> CompleteFace
completeFace c f =
    -- tl, tr, br, bl
    CompleteFace
        { tr = vs !! 1
        , tl = vs !! 0
        , br = vs !! 2
        , bl = vs !! 3
        }
  where
    as = angles f
    vs = fmap (\a -> fromJust (find ((a `elem`) . vertexToPos) (vertexes c))) as

connectFaces :: Cube -> CompleteFace -> CompleteCube
connectFaces c cfront@(CompleteFace{tr = ftr, tl = ftl, br = fbr, bl = fbl}) =
    CompleteCube
        { front = cfront
        , back = cback
        , left = cleft
        , right = cright
        , top = ctop
        , bottom = cbottom
        }
  where
    findOtherTwoVertex :: (Vertex, Vertex) -> (Vertex, Vertex) -> (Vertex, Vertex)
    findOtherTwoVertex (ov1, ov2) (v1, v2) =
        let allVertexes = concatMap vertexToPos [ov1, ov2, v1, v2]
            targetVetexes = concatMap vertexToPos [v1, v2]
            findVertex a = (head . filter ((a `elem`) . vertexToPos) . vertexes) c
            [nv1, nv2] =
                -- tl, tr, br, bl
                ( fmap findVertex
                    . (\\ targetVetexes)
                    . fromJust
                    . find (\fas -> length (fas \\ allVertexes) == 2 && length (fas \\ targetVetexes) == 2)
                    . fmap angles
                    . faces
                )
                    c
         in (nv1, nv2)
    (ctoptl, ctoptr) = findOtherTwoVertex (fbr, fbl) (ftr, ftl)
    (cbottomtr, cbottomtl) = findOtherTwoVertex (ftr, ftl) (fbr, fbl)
    (crighttr, crightbr) = findOtherTwoVertex (fbl, ftl) (fbr, ftr)
    (clefttl, cleftbl) = findOtherTwoVertex (ftr, fbr) (fbl, ftl)
    ctop = CompleteFace{tl = ctoptl, tr = ctoptr, br = ftr, bl = ftl}
    cright = CompleteFace{tl = ftr, tr = crighttr, br = crightbr, bl = fbr}
    cbottom = CompleteFace{tl = cbottomtl, tr = cbottomtr, br = fbr, bl = fbl}
    cleft = CompleteFace{tl = clefttl, tr = ftl, br = fbl, bl = cleftbl}
    cback = CompleteFace{tl = cbottomtl, tr = cbottomtr, br = ctoptr, bl = ctoptl}

wrapPosCube :: CompleteCube -> Map Position Field -> Position -> Position -> Direction -> (Position, Direction)
wrapPosCube c mf prevP p d = maybe (wrapAroundCube c prevP d) (const (p, d)) $ mf !? p

wrapAroundCube :: CompleteCube -> Position -> Direction -> (Position, Direction)
wrapAroundCube c prevP R = undefined
wrapAroundCube c prevP L = undefined
wrapAroundCube c prevP D = undefined
wrapAroundCube c prevP U = undefined

test = connectFaces cube cfront
  where
    faceSize = 4
    partialCube = (!! 1) $ buildPartialCubes (fst testInput) faceSize
    cube = (\cs -> mergeCubes (head cs) (tail cs)) $ buildPartialCubes (fst testInput) faceSize
    cfront = completeFace cube (head (faces cube))
test2 = do
    (mf, ms) <- input
    let faceSize = 50
        cube = (findLastVertex . (\cs -> mergeCubes (head cs) (tail cs))) (buildPartialCubes mf faceSize)
        cfront = completeFace cube (head (faces cube))
    return $ connectFaces cube cfront

-- 119103 too low
-- 129339 correct - final tile (83,128)
twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = do
    (mf, ms) <- input
    let faceSize = 50
        cube = (findLastVertex . (\cs -> mergeCubes (head cs) (tail cs))) (buildPartialCubes mf faceSize)
        cfront = completeFace cube (head (faces cube))
        completeCube = connectFaces cube cfront
    return $ solution (wrapPosCube completeCube mf) mf ms

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
