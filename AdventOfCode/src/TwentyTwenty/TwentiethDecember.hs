-------------------------------------------------------------------------------
--                           Advent Of Code - day 20                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentiethDecember where

import Data.Bifunctor (bimap, first, second)
import Data.List
  ( (\\)
  , delete
  , find
  , group
  , groupBy
  , maximumBy
  , partition
  , sort
  , transpose
  )
import qualified Data.Map as M
  ( Map
  , delete
  , difference
  , empty
  , filter
  , foldrWithKey
  , fromList
  , insert
  , intersection
  , keys
  , keysSet
  , lookup
  , mapKeys
  , toList
  , union
  , withoutKeys
  )
import Data.Maybe (catMaybes, fromJust, isJust)

type Coordinate = (Int, Int)

data Tile =
  Tile Int (M.Map Coordinate Bool)
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
  Tile tId $ M.mapKeys (\(x, y) -> (abs (y - 9), x)) content

flipTile :: Tile -> Tile
flipTile (Tile tId content) =
  Tile tId $ M.mapKeys (\(x, y) -> (abs (x - 9), y)) content

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
  (directions \\) .
  fmap (getOppositeDirection . snd) . M.keys . M.filter ((tId ==) . tileId)

input :: IO String
input = readFile "input/2020/20December.txt"

parseTiles :: String -> [Tile]
parseTiles s = do
  let rawTiles =
        (filter ((1 <) . length) . groupBy (\x y -> x /= "" && y /= "") . lines)
          s
  rawTile <- rawTiles
  let tId = ((\x -> read x :: Int) . init . dropWhile (' ' /=)) $ head rawTile
  (fmap (foldl1 (\(Tile x m1) (Tile _ m2) -> Tile x (m1 `M.union` m2))) .
   groupBy (\t1 t2 -> tileId t1 == tileId t2) .
   fmap
     (\(y, s') ->
        Tile
          tId
          (foldl
             (\m (x, c) ->
                if c == '#'
                  then M.insert (x, y) True m
                  else M.insert (x, y) False m)
             M.empty
             s')) .
   fmap (second (zip [0 ..])) . zip [0 ..] . tail)
    rawTile

tileEdges :: M.Map Coordinate Bool -> [[(Coordinate, Bool)]]
tileEdges t1 =
  let (t1EdgesWest, t1EdgesEst) =
        ((\l -> (take 10 l, drop (length l - 10) l)) . M.toList) t1
      (t1EdgesSouth, t1EdgesNorth) =
        ((\l -> (fmap (l !!) [9,19 .. 99], fmap (l !!) [0,10 .. 90])) . M.toList)
          t1
   in [t1EdgesWest, t1EdgesSouth, t1EdgesEst, t1EdgesNorth]

showRow :: [Bool] -> String
showRow =
  foldl
    (\s x ->
       if x
         then s ++ "#"
         else s ++ ".")
    ""

showTile :: Tile -> String
showTile (Tile tId content) =
  "Tile " ++
  show tId ++
  ":\n" ++
  (foldl (\s r -> s ++ showRow (fmap snd r) ++ "\n") "" .
   transpose . groupBy (\((x, _), _) ((x', _), _) -> x == x') . M.toList)
    content

solution1 :: String -> Int
solution1 =
  product .
  fmap tileId .
  getCorners .
  (\ts -> buildTileDatabase [head ts] (tail ts) M.empty) . parseTiles

findNeighbor :: [Tile] -> (Direction, Row) -> TileDb -> Maybe (Tile, Direction)
findNeighbor [] _ _ = Nothing
findNeighbor (t:ts) edge tDb =
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
          else take 4 (iterate rotateUp t) >>= \t' -> t' : [flipTile t']
      matchTile =
        find
          (\t' ->
             fmap snd edge ==
             (fmap snd . snd . head)
               (getEdges t' [getOppositeDirection edgeDirection]))
          tileConfigurations
   in (\t' -> (t', edgeDirection)) <$> matchTile

getCorners :: TileDb -> [Tile]
getCorners tDb =
  let cornersId =
        (fmap head .
         filter ((4 ==) . length) .
         group .
         sort . M.foldrWithKey (\((tId, tId'), _) _ acc -> tId : tId' : acc) [])
          tDb
   in (\tId -> fromJust (M.lookup ((tId, tId), Self) tDb)) <$> cornersId

buildImage :: TileDb -> Tile
buildImage tdb = undefined

updateTileDb :: TileId -> TileDb -> [(Tile, Direction)] -> TileDb
updateTileDb _ tDb [] = tDb
updateTileDb tId tDb ((n, nd):ns) =
  let tDb' = M.insert ((tId, tileId n), nd) n tDb
      tDb'' =
        if isJust (M.lookup ((tileId n, tileId n), Self) tDb')
          then tDb'
          else M.insert ((tileId n, tileId n), Self) n tDb'
   in updateTileDb tId tDb'' ns

buildTileDatabase :: [Tile] -> [Tile] -> TileDb -> TileDb
buildTileDatabase [] [] tDb = tDb
buildTileDatabase [t] [] tDb = M.insert ((tileId t, tileId t), Self) t tDb
buildTileDatabase [] (t:ts) tDb = buildTileDatabase [t] ts tDb
buildTileDatabase (t:ts) freeTiles tDb =
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
      newTDb = updateTileDb (tileId t) tDb neighboors
      newTDb' =
        if isJust maybeTileSelf
          then newTDb
          else M.insert
                 ((tileId tileSelf, tileId tileSelf), Self)
                 tileSelf
                 newTDb
      fixedTilesToLoop = ts ++ fmap fst neighboors
   in buildTileDatabase fixedTilesToLoop freeTiles' newTDb'

seaMonster :: Tile
seaMonster = undefined

searchSeaMonster :: Tile -> [(Tile, Int)]
searchSeaMonster image = undefined

killSeaMonsters :: Tile -> Tile
killSeaMonsters image = undefined

countRoughWater :: Tile -> Int
countRoughWater t = undefined

solution2 :: String -> Int
solution2 s =
  let ts = parseTiles s
      tileDatabase = buildTileDatabase [head ts] (tail ts) M.empty
      image = buildImage tileDatabase
      mostSeaMonsterConfiguration =
        fst . maximumBy (\(_, m) (_, m') -> m `compare` m') $
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
