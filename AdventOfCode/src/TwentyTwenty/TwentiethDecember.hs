{-# LANGUAGE TupleSections #-}

-------------------------------------------------------------------------------
--                           Advent Of Code - day 20                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentiethDecember where

import Data.Bifunctor (second)
import Data.List (
    find,
    group,
    groupBy,
    maximumBy,
    sort,
    transpose,
    (\\),
 )
import qualified Data.Map as M (
    Map,
    empty,
    filter,
    filterWithKey,
    findMax,
    findMin,
    foldrWithKey,
    insert,
    isSubmapOf,
    keys,
    lookup,
    mapKeys,
    mapWithKey,
    member,
    partitionWithKey,
    singleton,
    size,
    toList,
    union,
 )
import Data.Maybe (catMaybes, fromJust, isJust)

type Coordinate = (Int, Int)

data Tile
    = Tile Int (M.Map Coordinate Bool)
    deriving (Show)

type TileId = Int

type Row = [(Coordinate, Bool)]

data Direction
    = W
    | S
    | E
    | N
    | Self
    deriving (Eq, Show, Ord)

type TileDb = M.Map ((TileId, TileId), Direction) Tile

tileId :: Tile -> Int
tileId (Tile i _) = i

tileContent :: Tile -> M.Map Coordinate Bool
tileContent (Tile _ c) = c

rotateUp :: Tile -> Tile
rotateUp (Tile tId content) =
    let maxY = (snd . fst) (M.findMax content)
     in Tile tId $ M.mapKeys (\(x, y) -> (abs (y - maxY), x)) content

flipTile :: Tile -> Tile
flipTile (Tile tId content) =
    let maxY = (snd . fst) (M.findMax content)
     in Tile tId $ M.mapKeys (\(x, y) -> (abs (x - maxY), y)) content

directionToInt :: Direction -> Int
directionToInt W = 0
directionToInt S = 1
directionToInt E = 2
directionToInt N = 3
directionToInt Self = -1

directions :: [Direction]
directions = [W, S, E, N]

getOppositeDirection :: Direction -> Direction
getOppositeDirection W = E
getOppositeDirection S = N
getOppositeDirection E = W
getOppositeDirection N = S
getOppositeDirection Self = Self

getEdges :: Tile -> [Direction] -> [(Direction, Row)]
getEdges (Tile _ tc) = fmap (\d -> (d, tileEdges tc !! directionToInt d))

howManyOpenEdges :: Tile -> TileDb -> [Direction]
howManyOpenEdges (Tile tId _) =
    (directions \\)
        . fmap (getOppositeDirection . snd)
        . M.keys
        . M.filter ((tId ==) . tileId)

input :: IO String
input = readFile "input/2020/20December.txt"

parseTiles :: String -> [Tile]
parseTiles s = do
    let rawTiles =
            (filter ((1 <) . length) . groupBy (\x y -> x /= "" && y /= "") . lines)
                s
    rawTile <- rawTiles
    let tId = ((\x -> read x :: Int) . init . dropWhile (' ' /=)) $ head rawTile
    ( fmap (foldl1 (\(Tile x m1) (Tile _ m2) -> Tile x (m1 `M.union` m2)))
            . groupBy (\t1 t2 -> tileId t1 == tileId t2)
            . fmap
                ( ( \(y, s') ->
                        Tile
                            tId
                            ( foldl
                                ( \m (x, c) ->
                                    if c == '#'
                                        then M.insert (x, y) True m
                                        else M.insert (x, y) False m
                                )
                                M.empty
                                s'
                            )
                  )
                    . second (zip [0 ..])
                )
            . zip [0 ..]
            . tail
        )
        rawTile

tileEdges :: M.Map Coordinate Bool -> [[(Coordinate, Bool)]]
tileEdges t1 =
    let (t1EdgesWest, t1EdgesEst) =
            ((\l -> (take 10 l, drop (length l - 10) l)) . M.toList) t1
        (t1EdgesSouth, t1EdgesNorth) =
            ((\l -> (fmap (l !!) [9, 19 .. 99], fmap (l !!) [0, 10 .. 90])) . M.toList)
                t1
     in [t1EdgesWest, t1EdgesSouth, t1EdgesEst, t1EdgesNorth]

showRow :: [Bool] -> String
showRow =
    foldl
        ( \s x ->
            if x
                then s ++ "#"
                else s ++ "."
        )
        ""

showTile :: Tile -> String
showTile (Tile tId content) =
    "Tile "
        ++ show tId
        ++ ":\n"
        ++ ( foldl (\s r -> s ++ showRow (fmap snd r) ++ "\n") ""
                . transpose
                . groupBy (\((x, _), _) ((x', _), _) -> x == x')
                . M.toList
           )
            content

solution1 :: String -> Int
solution1 =
    product
        . fmap tileId
        . getCorners
        . (\ts -> buildTileDatabase [head ts] (tail ts) M.empty)
        . parseTiles

findNeighbor :: [Tile] -> (Direction, Row) -> TileDb -> Maybe (Tile, Direction)
findNeighbor [] _ _ = Nothing
findNeighbor (t : ts) edge tDb =
    let maybeNeighbor = isNeighbor edge t tDb
     in if isJust maybeNeighbor
            then maybeNeighbor
            else findNeighbor ts edge tDb

isNeighbor :: (Direction, Row) -> Tile -> TileDb -> Maybe (Tile, Direction)
isNeighbor (edgeDirection, edge) t tDb =
    let maybeTile = M.lookup ((tileId t, tileId t), Self) tDb
        tileConfigurations =
            if isJust maybeTile
                then [fromJust maybeTile]
                else allTileConfigurations t
        matchTile =
            find
                ( \t' ->
                    fmap snd edge
                        == (fmap snd . snd . head)
                            (getEdges t' [getOppositeDirection edgeDirection])
                )
                tileConfigurations
     in (,edgeDirection) <$> matchTile

allTileConfigurations :: Tile -> [Tile]
allTileConfigurations t =
    take 4 (iterate rotateUp t) >>= \t' -> t' : [flipTile t']

getCorners :: TileDb -> [Tile]
getCorners tDb =
    let cornersId =
            ( fmap head
                . filter ((4 ==) . length)
                . group
                . sort
                . M.foldrWithKey (\((tId, tId'), _) _ acc -> tId : tId' : acc) []
            )
                tDb
     in (\tId -> fromJust (M.lookup ((tId, tId), Self) tDb)) <$> cornersId

buildImage :: TileDb -> Tile
buildImage tDb =
    let (_, connections) =
            M.partitionWithKey (\(_, direction) _ -> direction == Self) tDb
        imageGrid = M.singleton (0, 0) $ getTopLeftCorner tDb
        imageGrid' = buildImageGrid imageGrid (M.toList connections)
     in imageGridToTile imageGrid' False

getTopLeftCorner :: TileDb -> Tile
getTopLeftCorner tDb =
    let keys = fst <$> (M.keys . M.filterWithKey (\(_, d) _ -> d /= Self)) tDb
        topLeftCornerId =
            head $
                foldl
                    (\allKeys (_, tId') -> filter (tId' /=) allKeys)
                    (fmap fst keys)
                    keys
        topLeftCornerTile =
            M.lookup ((topLeftCornerId, topLeftCornerId), Self) tDb
     in fromJust topLeftCornerTile

buildImageGrid ::
    M.Map Coordinate Tile ->
    [(((TileId, TileId), Direction), Tile)] ->
    M.Map Coordinate Tile
buildImageGrid imageGrid [] = imageGrid
buildImageGrid imageGrid (c@(((tIdSource, _), direction), tile) : connections) =
    let imageGridCell = M.filter (\t -> tileId t == tIdSource) imageGrid
     in M.foldrWithKey
            ( \(x, y) _ _ ->
                case direction of
                    S ->
                        buildImageGrid (M.insert (x, y + 1) tile imageGrid) connections
                    E ->
                        buildImageGrid (M.insert (x + 1, y) tile imageGrid) connections
                    _ -> error "direction not supported"
            )
            (buildImageGrid imageGrid (connections ++ [c]))
            imageGridCell

imageGridToTile :: M.Map Coordinate Tile -> Bool -> Tile
imageGridToTile image withEdges =
    M.foldrWithKey
        ( \(x, y) t (Tile tId tc) ->
            let (offsetX, offsetY) =
                    ( if withEdges
                        then 10
                        else 8
                    , if withEdges
                        then 10
                        else 8
                    )
                (x', y') = (offsetX * x, offsetY * y)
                tileContentToAdd =
                    if withEdges
                        then t
                        else tileRemoveEdges t
                tileContentToAdd' =
                    M.mapKeys
                        (\(tx, ty) -> (tx + x', ty + y'))
                        (tileContent tileContentToAdd)
             in Tile tId (M.union tc tileContentToAdd')
        )
        (Tile 0 M.empty)
        image

updateTileDb :: Tile -> TileDb -> [(Tile, Direction)] -> TileDb
updateTileDb _ tDb [] = tDb
updateTileDb t tDb ((n, nd) : ns) =
    let tDb' = insertOnlyEstSouth t (n, nd) tDb
        tDb'' =
            if isJust (M.lookup ((tileId n, tileId n), Self) tDb')
                then tDb'
                else M.insert ((tileId n, tileId n), Self) n tDb'
     in updateTileDb t tDb'' ns
  where
    insertOnlyEstSouth :: Tile -> (Tile, Direction) -> TileDb -> TileDb
    insertOnlyEstSouth tile (neigh, neighDir) m
        | neighDir == N = M.insert ((tileId neigh, tileId tile), S) tile m
        | neighDir == W = M.insert ((tileId neigh, tileId tile), E) tile m
        | otherwise = M.insert ((tileId tile, tileId neigh), neighDir) n m

buildTileDatabase :: [Tile] -> [Tile] -> TileDb -> TileDb
buildTileDatabase [] [] tDb = tDb
buildTileDatabase [t] [] tDb = M.insert ((tileId t, tileId t), Self) t tDb
buildTileDatabase [] (t : ts) tDb = buildTileDatabase [t] ts tDb
buildTileDatabase (t : ts) freeTiles tDb =
    let maybeTileSelf = M.lookup ((tileId t, tileId t), Self) tDb
        tileSelf = foldr const t maybeTileSelf
        freeTiles' = filter (\x -> tileId x /= tileId tileSelf) freeTiles
        openEdges = getEdges tileSelf $ howManyOpenEdges tileSelf tDb
        neighboors =
            catMaybes $
                foldl
                    (\acc edge -> acc ++ [findNeighbor freeTiles' edge tDb])
                    []
                    openEdges
        newTDb = updateTileDb t tDb neighboors
        newTDb' =
            if isJust maybeTileSelf
                then newTDb
                else
                    M.insert
                        ((tileId tileSelf, tileId tileSelf), Self)
                        tileSelf
                        newTDb
        fixedTilesToLoop = ts ++ fmap fst neighboors
     in buildTileDatabase fixedTilesToLoop freeTiles' newTDb'

tileRemoveEdges :: Tile -> Tile
tileRemoveEdges (Tile tId tc) =
    let ((minTcX, minTcY), (maxTcX, maxTcY)) =
            ((fst . M.findMin) tc, (fst . M.findMax) tc)
     in Tile tId $
            M.filterWithKey
                ( \(x, y) _ ->
                    (x /= minTcX && x /= maxTcX) && (y /= minTcY && y /= maxTcY)
                )
                tc

seaMonster :: Tile
seaMonster =
    let rawSeaMonster =
            head $
                parseTiles
                    "Tile 666:\n\
                    \..................#.\n\
                    \#....##....##....###\n\
                    \.#..#..#..#..#..#..."
     in Tile (tileId rawSeaMonster) $ M.filter id (tileContent rawSeaMonster)

searchSeaMonster :: Tile -> [(Tile, [M.Map Coordinate Bool])]
searchSeaMonster image = do
    t <- allTileConfigurations image
    let tc = tileContent t
        monsterContent = tileContent seaMonster
        ((minTcX, minTcY), (maxTcX, maxTcY)) =
            ((fst . M.findMin) tc, (fst . M.findMax) tc)
        (maxMonsterX, maxMonsterY) = (20, 3)
        monsters =
            ( \(offsetX, offsetY) ->
                M.mapKeys (\(x, y) -> (x + offsetX, y + offsetY)) monsterContent
            )
                <$> [ (a, b)
                    | a <- [minTcX .. (maxTcX - maxMonsterX)]
                    , b <- [minTcY .. (maxTcY - maxMonsterY)]
                    ]
        monstersFound =
            foldl
                ( \acc m ->
                    if M.isSubmapOf m tc
                        then acc ++ [m]
                        else acc
                )
                []
                monsters
    return (t, monstersFound)

killSeaMonsters :: (Tile, [M.Map Coordinate Bool]) -> Tile
killSeaMonsters (image, []) = image
killSeaMonsters (Tile tId tc, m : ms) =
    killSeaMonsters
        ( Tile
            tId
            ( M.mapWithKey
                ( \k b ->
                    not (k `M.member` m) && b
                )
                tc
            )
        , ms
        )

countRoughWater :: Tile -> Int
countRoughWater (Tile _ tc) = M.size $ M.filter id tc

solution2 :: String -> Int
solution2 s =
    let ts = parseTiles s
        tileDatabase = buildTileDatabase [head ts] (tail ts) M.empty
        image = buildImage tileDatabase
        mostSeaMonsterConfiguration =
            maximumBy (\(_, m) (_, m') -> length m `compare` length m') $
                searchSeaMonster image
        waters = killSeaMonsters mostSeaMonsterConfiguration
     in countRoughWater waters

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution2 <$> input

-- test data ------------------------------------------------------------
testSolution1 :: Int
testSolution1 = solution1 testInput

testSolution2 :: Int
testSolution2 = solution2 testInput

testInput :: String
testInput =
    "Tile 2311:\n\
    \..##.#..#.\n\
    \##..#.....\n\
    \#...##..#.\n\
    \####.#...#\n\
    \##.##.###.\n\
    \##...#.###\n\
    \.#.#.#..##\n\
    \..#....#..\n\
    \###...#.#.\n\
    \..###..###\n\
    \\n\
    \Tile 1951:\n\
    \#.##...##.\n\
    \#.####...#\n\
    \.....#..##\n\
    \#...######\n\
    \.##.#....#\n\
    \.###.#####\n\
    \###.##.##.\n\
    \.###....#.\n\
    \..#.#..#.#\n\
    \#...##.#..\n\
    \\n\
    \Tile 1171:\n\
    \####...##.\n\
    \#..##.#..#\n\
    \##.#..#.#.\n\
    \.###.####.\n\
    \..###.####\n\
    \.##....##.\n\
    \.#...####.\n\
    \#.##.####.\n\
    \####..#...\n\
    \.....##...\n\
    \\n\
    \Tile 1427:\n\
    \###.##.#..\n\
    \.#..#.##..\n\
    \.#.##.#..#\n\
    \#.#.#.##.#\n\
    \....#...##\n\
    \...##..##.\n\
    \...#.#####\n\
    \.#.####.#.\n\
    \..#..###.#\n\
    \..##.#..#.\n\
    \\n\
    \Tile 1489:\n\
    \##.#.#....\n\
    \..##...#..\n\
    \.##..##...\n\
    \..#...#...\n\
    \#####...#.\n\
    \#..#.#.#.#\n\
    \...#.#.#..\n\
    \##.#...##.\n\
    \..##.##.##\n\
    \###.##.#..\n\
    \\n\
    \Tile 2473:\n\
    \#....####.\n\
    \#..#.##...\n\
    \#.##..#...\n\
    \######.#.#\n\
    \.#...#.#.#\n\
    \.#########\n\
    \.###.#..#.\n\
    \########.#\n\
    \##...##.#.\n\
    \..###.#.#.\n\
    \\n\
    \Tile 2971:\n\
    \..#.#....#\n\
    \#...###...\n\
    \#.#.###...\n\
    \##.##..#..\n\
    \.#####..##\n\
    \.#..####.#\n\
    \#..#.#..#.\n\
    \..####.###\n\
    \..#.#.###.\n\
    \...#.#.#.#\n\
    \\n\
    \Tile 2729:\n\
    \...#.#.#.#\n\
    \####.#....\n\
    \..#.#.....\n\
    \....#..#.#\n\
    \.##..##.#.\n\
    \.#.####...\n\
    \####.#.#..\n\
    \##.####...\n\
    \##..#.##..\n\
    \#.##...##.\n\
    \\n\
    \Tile 3079:\n\
    \#.#.#####.\n\
    \.#..######\n\
    \..#.......\n\
    \######....\n\
    \####.#..#.\n\
    \.#...#.##.\n\
    \#.#####.##\n\
    \..#.###...\n\
    \..#.......\n\
    \..#.###..."
