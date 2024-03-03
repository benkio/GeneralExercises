{-# LANGUAGE TupleSections #-}

module TwentyTwentyTwo.TwentySecondDecember where

import Control.Applicative (Alternative ((<|>)))
import Control.Arrow ((&&&))
import Data.Bifunctor (bimap, first)
import Data.Char (isDigit)
import Data.Functor (($>), (<&>))
import Data.List (cycle, delete, elemIndex, find, minimumBy, notElem, nub, nubBy, partition, (\\))
import Data.Map (Map, fromList, keys, member, notMember, (!), (!?))
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Text.Printf

data Field = Empty | Wall deriving (Eq, Show)
type Position = (Int, Int)
data Move = ML | MR | M Int deriving (Eq, Show)
data Direction = R | D | L | U deriving (Show, Eq, Ord)

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
startingPoint mf = head $ mapMaybe (\x -> ((x, 0), R) <$ (mf !? (x, 0))) [0 ..]

applyMoves :: Map Position Field -> [Move] -> (Position -> Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMoves _ [] wrapF pos dir = (pos, dir)
applyMoves mf (m : ms) wrapF pos dir = applyMoves mf ms wrapF newPos newDir
  where
    (newPos, newDir) =
        applyMove mf m wrapF pos dir

changeDir :: Int -> Direction -> Direction
changeDir increment d = ds !! ((i + increment) `mod` length ds)
  where
    ds = [R, D, L, U]
    i = fromJust $ elemIndex d ds

applyMove :: Map Position Field -> Move -> (Position -> Position -> Direction -> (Position, Direction)) -> Position -> Direction -> (Position, Direction)
applyMove _ ML wrapF pos dir = (pos, changeDir (-1) dir)
applyMove _ MR wrapF pos dir = (pos, changeDir 1 dir)
applyMove mf (M steps) wrapF pos dir
    | steps == 0 || checkCollision newP = (pos, dir)
    | otherwise = applyMove mf (M (steps - 1)) wrapF newP newDir
  where
    (newP, newDir) = wrapF pos (moveInDirection pos dir) dir
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
data CompleteFace = CompleteFace
    { tl :: (Position, Vertex)
    , tr :: (Position, Vertex)
    , br :: (Position, Vertex)
    , bl :: (Position, Vertex)
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
findKey mf k = (mf !? k) $> k
completeFaceAngles cf = [(fst . tl) cf, (fst . tr) cf, (fst . br) cf, (fst . bl) cf]
completeFaceVertexes cf = [(snd . tl) cf, (snd . tr) cf, (snd . br) cf, (snd . bl) cf]
completeFaceEdges = genEdges . completeFaceAngles
  where
    genEdges [] = []
    genEdges ((x, y) : xs) = (fmap (\(x', y') -> [(a, b) | a <- [min x x' .. max x x'], b <- [min y y' .. max y y']]) . filter (\(x', y') -> x == x' || y == y')) xs ++ genEdges xs
completeCubeFaces c = [front c, top c, right c, bottom c, left c, back c]

mergeIncompleteVertexes cvs [] = cvs
mergeIncompleteVertexes cvs (v : vs)
    | any (\cv -> all (`elem` vertexToPos cv) (vertexToPos v)) cvs = mergeIncompleteVertexes cvs vs
    | isJust findOverlap =
        let newVertexPositions = ((\x -> nub (vertexToPos x ++ vertexToPos v)) . fromJust) findOverlap
            newVertex = Vertex{v1 = head newVertexPositions, v2 = newVertexPositions !! 1, v3 = newVertexPositions !! 2}
            vs' = ((`delete` vs) . fromJust) findOverlap
         in newVertex : mergeIncompleteVertexes cvs vs'
    | otherwise = v : mergeIncompleteVertexes cvs vs
  where
    findOverlap = find (\x -> any (`elem` vertexToPos x) (vertexToPos v)) vs

findEdge :: Map Position Field -> Int -> Position -> Bool
findEdge mf target (x, y) = ((== target) . length) keyAnglePattern
  where
    keyAnglePattern = filter (`member` mf) [(a, b) | a <- [(x - 1) .. (x + 1)], b <- [(y - 1) .. (y + 1)]]

buildPartialCubes :: Map Position Field -> Int -> [Cube]
buildPartialCubes mf faceSize = (fmap toPartialCube . filter (findEdge mf 8) . keys) mf
  where
    toPartialCube (x, y)
        | (x - 1, y - 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (-1, -1), faces = [buildFace faceSize (x, y) TL, buildFace faceSize (x - 1, y) TR, buildFace faceSize (x, y - 1) BL]}
        | (x + 1, y - 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (1, -1), faces = [buildFace faceSize (x, y) TR, buildFace faceSize (x + 1, y) TL, buildFace faceSize (x, y - 1) BR]}
        | (x - 1, y + 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (-1, 1), faces = [buildFace faceSize (x, y) BL, buildFace faceSize (x - 1, y) BR, buildFace faceSize (x, y + 1) TL]}
        | (x + 1, y + 1) `notMember` mf = Cube{vertexes = buildVertexes (x, y) (1, 1), faces = [buildFace faceSize (x, y) BR, buildFace faceSize (x + 1, y) BL, buildFace faceSize (x, y + 1) TR]}
    buildVertexes (x, y) (dx, dy) =
        [ Vertex{v1 = (x, y), v2 = (x + signum dx, y), v3 = (x, y + signum dy)}
        , IncompleteVertex{v1 = (x, y + (faceSize - 1) * negate (signum dy)), v2 = (x + signum dx, y + (faceSize - 1) * negate (signum dy))}
        , IncompleteVertex{v1 = (x + (faceSize - 1) * negate (signum dx), y), v2 = (x + (faceSize - 1) * negate (signum dx), y + signum dy)}
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
        then c{vertexes = lostVertex : (filter (not . vertexIsIncomplete) . vertexes) c}
        else c
  where
    es = (concatMap angles . faces) c
    completeVertexes = (filter (not . vertexIsIncomplete) . vertexes) c
    vs = concatMap vertexToPos completeVertexes
    [v', v'', v'''] = es \\ vs
    lostVertex = Vertex{v1 = v', v2 = v'', v3 = v'''}

completeFace :: Cube -> Face -> CompleteFace
completeFace c f =
    CompleteFace
        { tr = vs !! 1
        , tl = head vs
        , br = vs !! 2
        , bl = vs !! 3
        }
  where
    as = angles f
    vs = fmap (\a -> (a, fromJust (find ((a `elem`) . vertexToPos) (vertexes c)))) as

connectFaces :: Cube -> CompleteFace -> CompleteCube
connectFaces c cfront@(CompleteFace{tr = (pftr, ftr), tl = (pftl, ftl), br = (pfbr, fbr), bl = (pfbl, fbl)}) =
    CompleteCube
        { front = cfront
        , back = cback
        , left = cleft
        , right = cright
        , top = ctop
        , bottom = cbottom
        }
  where
    findOtherTwoVertex :: [Position] -> (Vertex, Vertex) -> ((Position, Vertex), (Position, Vertex), (Position, Vertex), (Position, Vertex))
    findOtherTwoVertex currentFaceOtherPositions (v1, v2) =
        let targetVetexes = concatMap vertexToPos [v1, v2]
            findVertex a = (head . filter ((a `elem`) . vertexToPos) . vertexes) c
            [tl, tr, br, bl] =
                ( fromJust
                    . find (\fas -> length (fas \\ (targetVetexes ++ currentFaceOtherPositions)) == 2)
                    . fmap angles
                    . faces
                )
                    c
         in ((tl, findVertex tl), (tr, findVertex tr), (br, findVertex br), (bl, findVertex bl))
    findBackFace (ps, vs) =
        let facesPoints = (fmap angles . faces) c
            maybeMissingFace = find (any (`notElem` ps)) facesPoints
            [tl, tr, br, bl] = fromMaybe findBackByVertex maybeMissingFace
            findBackByVertex :: [Position]
            findBackByVertex = ((\\ concat facesPoints) . nub . concatMap vertexToPos) vs
            findVertex a = (head . filter ((a `elem`) . vertexToPos) . vertexes) c
         in ((tl, findVertex tl), (tr, findVertex tr), (br, findVertex br), (bl, findVertex bl))
    (ctoptl, ctoptr, ctopbr, ctopbl) = findOtherTwoVertex [pfbr, pfbl] (ftr, ftl)
    (cbottomtl, cbottomtr, cbottombr, cbottombl) = findOtherTwoVertex [pftr, pftl] (fbr, fbl)
    (crighttl, crighttr, crightbr, crightbl) = findOtherTwoVertex [pfbl, pftl] (fbr, ftr)
    (clefttl, clefttr, cleftbr, cleftbl) = findOtherTwoVertex [pftr, pfbr] (fbl, ftl)
    allStuff = [(pftr, ftr), (pftl, ftl), (pfbr, fbr), (pfbl, fbl), ctoptl, ctoptr, ctopbr, ctopbl, cbottomtl, cbottomtr, cbottombr, cbottombl, crighttl, crighttr, crightbr, crightbl, clefttl, clefttr, cleftbr, cleftbl]
    (cbacktl, cbacktr, cbackbr, cbackbl) = findBackFace $ unzip allStuff

    ctop = CompleteFace{tl = ctoptl, tr = ctoptr, br = ctopbr, bl = ctopbl}
    cright = CompleteFace{tl = crighttl, tr = crighttr, br = crightbr, bl = crightbl}
    cbottom = CompleteFace{tl = cbottomtl, tr = cbottomtr, br = cbottombr, bl = cbottombl}
    cleft = CompleteFace{tl = clefttl, tr = clefttr, br = cleftbr, bl = cleftbl}
    cback = CompleteFace{tl = cbacktl, tr = cbacktr, br = cbackbr, bl = cbackbl}

wrapPosCube :: CompleteCube -> Map Position Field -> Position -> Position -> Direction -> (Position, Direction)
wrapPosCube c mf prevP p d
    | isVertex prevP && p `notMember` mf = wrapAroundCubeVertex c mf prevP p d
    | otherwise = maybe ((\newP -> (newP, findNewDirection mf newP)) (wrapAroundCube c prevP d)) (const (p, d)) $ mf !? p
  where
    isVertex p = p `elem` (nub . concatMap completeFaceAngles . completeCubeFaces) c

wrapAroundCubeVertex :: CompleteCube -> Map Position Field -> Position -> Position -> Direction -> (Position, Direction)
wrapAroundCubeVertex c mf prevP p d = first (fromJust . find isVertex . decrementFuncton) $ wrapPosCube c mf (incrementFunction prevP) p d
  where
    startingFace = (fromJust . find ((prevP `elem`) . concat . completeFaceEdges) . completeCubeFaces) c
    edges = (filter (prevP `elem`) . completeFaceEdges) startingFace
    isVertex p = p `elem` (nub . concatMap completeFaceAngles . completeCubeFaces) c
    (incrementFunction, decrementFuncton) = genIncrementFunc edges prevP d

genIncrementFunc :: [[Position]] -> Position -> Direction -> (Position -> Position, Position -> [Position])
genIncrementFunc edges p d
    | d == R || d == L = (const nextY, \(x, y) -> [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)])
    | d == U || d == D = (const nextX, \(x, y) -> [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)])
  where
    nextY = ((!! 1) . fromJust . find (\e -> abs (snd (e !! 1) - snd p) == 1) . fmap (\e -> if head e == p then e else reverse e)) edges
    nextX = ((!! 1) . fromJust . find (\e -> abs (fst (e !! 1) - fst p) == 1) . fmap (\e -> if head e == p then e else reverse e)) edges

wrapAroundCube :: CompleteCube -> Position -> Direction -> Position
wrapAroundCube c prevP dir = result
  where
    startingFace = (fromJust . find ((prevP `elem`) . concat . completeFaceEdges) . completeCubeFaces) c
    startingEdge = (fromJust . find (prevP `elem`) . completeFaceEdges) startingFace
    indexFromVertex = fromJust $ elemIndex prevP startingEdge
    v1 = (fromJust . find ((head startingEdge `elem`) . vertexToPos) . completeFaceVertexes) startingFace
    v2 = (fromJust . find ((last startingEdge `elem`) . vertexToPos) . completeFaceVertexes) startingFace
    targetFace = (fromJust . find ((== 2) . length . (\\ (concatMap vertexToPos [v1, v2] ++ completeFaceAngles startingFace)) . completeFaceAngles) . completeCubeFaces) c
    (x, y) = (head . filter (`elem` completeFaceAngles targetFace) . vertexToPos) v1
    (x', y') = (head . filter (`elem` completeFaceAngles targetFace) . vertexToPos) v2
    targetEdge = [(a, b) | a <- [min x x' .. max x x'], b <- [min y y' .. max y y']]
    result = if head targetEdge == (x, y) then targetEdge !! indexFromVertex else reverse targetEdge !! indexFromVertex

findNewDirection :: Map Position Field -> Position -> Direction
findNewDirection mf (x, y)
    | neighboorOut == [False, True, True, True] = L
    | neighboorOut == [True, False, True, True] = R
    | neighboorOut == [True, True, False, True] = U
    | neighboorOut == [True, True, True, False] = D
  where
    neighboorOut = fmap (`member` mf) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

test = solution (wrapPosCube completeCube mf) mf ms
  where
    faceSize = 4
    (mf, ms) = testInput
    partialCube = (!! 1) $ buildPartialCubes mf faceSize
    cube = (\cs -> mergeCubes (head cs) (tail cs)) $ buildPartialCubes mf faceSize
    cfront = completeFace cube (head (faces cube))
    completeCube = connectFaces cube cfront
test2 = do
    (mf, ms) <- input
    let faceSize = 50
        cube = (findLastVertex . (\cs -> mergeCubes (head cs) (tail cs))) (buildPartialCubes mf faceSize)
        cfront = completeFace cube (head (faces cube))
    return $ completeCubeFaces $ connectFaces cube cfront

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
