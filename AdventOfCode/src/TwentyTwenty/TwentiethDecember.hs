-------------------------------------------------------------------------------
--                           Advent Of Code - day 20                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentiethDecember where

import           Data.Bifunctor (bimap, first, second)
import           Data.List      (delete, find, group, groupBy, maximumBy,
                                 partition, sort, transpose, (\\))
import qualified Data.Map       as M (Map, delete, difference, empty, filter,
                                      foldrWithKey, fromList, insert,
                                      intersection, keys, keysSet, lookup,
                                      mapKeys, toList, union, withoutKeys)
import           Data.Maybe     (catMaybes, fromJust, isJust)

type Coordinate = (Int, Int)

data Tile =
  Tile Int (M.Map Coordinate Bool)
  deriving (Show)

tileId :: Tile -> Int
tileId (Tile i _) = i

tileContent :: Tile -> M.Map Coordinate Bool
tileContent (Tile _ c) = c

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

addReverseEdges ::
     (Int, [[(Coordinate, Bool)]]) -> (Int, [[(Coordinate, Bool)]])
addReverseEdges = second (\x -> x >>= \l -> [l, reverse l])

stripCoordinates :: (Int, [[(Coordinate, Bool)]]) -> (Int, [[Bool]])
stripCoordinates = second ((fmap . fmap) snd)

removeDuplicatesEdgesInTailes ::
     [(Int, [[(Coordinate, Bool)]])]
  -> [(Int, [[(Coordinate, Bool)]])]
  -> [(Int, [[(Coordinate, Bool)]])]
removeDuplicatesEdgesInTailes [] acc = acc
removeDuplicatesEdgesInTailes (x:xs) acc =
  let (newEdges, newAcc) = removeDuplicateEdges (xs, acc) x
   in removeDuplicatesEdgesInTailes newEdges newAcc

removeDuplicateEdges ::
     ([(Int, [[(Coordinate, Bool)]])], [(Int, [[(Coordinate, Bool)]])])
  -> (Int, [[(Coordinate, Bool)]])
  -> ([(Int, [[(Coordinate, Bool)]])], [(Int, [[(Coordinate, Bool)]])])
removeDuplicateEdges (resultEdgeToCheck, standaloneEdges) (tId, resultEdge) =
  let (matches, notMatches) =
        partition
          (\x ->
             fmap snd x `elem` (resultEdgeToCheck >>= (snd . stripCoordinates)))
          resultEdge
      resultEdgeToCheckNoDuplicates =
        second
          (filter
             (\x ->
                fmap snd x `notElem`
                (fmap . fmap) snd (matches ++ fmap reverse matches))) <$>
        resultEdgeToCheck
   in (resultEdgeToCheckNoDuplicates, standaloneEdges ++ [(tId, notMatches)])

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
solution1 = product . fmap fst . cornerTiles

cornerTiles :: String -> [(Int, [[(Coordinate, Bool)]])]
cornerTiles s =
  let tiles = parseTiles s
      resultEdge =
        addReverseEdges . (\t -> (tileId t, tileEdges (tileContent t))) <$>
        tiles
      corners =
        (filter ((2 <) . length . snd) . (`removeDuplicatesEdgesInTailes` []))
          resultEdge
   in corners

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

-- Solution2 -------------------------------------
type TileId = Int

type Row = [(Coordinate, Bool)]

data Direction
  = W
  | S
  | E
  | N
  | Self
  deriving (Eq, Show, Ord)

type TileDb = M.Map (TileId, Direction) Tile

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

directions :: [Direction]
directions = [W, S, E, N]

getEdges :: Tile -> [Direction] -> [(Direction, Row)]
getEdges (Tile _ tc) = fmap (\d -> (d, tileEdges tc !! directionToInt d))

howManyOpenEdges :: Tile -> TileDb -> [Direction]
howManyOpenEdges (Tile tId _) =
  (directions \\) . fmap snd . M.keys . M.filter ((tId ==) . tileId)

findNeighbor :: [Tile] -> (Direction, Row) -> Maybe (Tile, Direction)
findNeighbor [] _ = Nothing
findNeighbor (t:ts) edge =
  let maybeNeighbor = isNeighbor edge t
  in if isJust maybeNeighbor then maybeNeighbor else findNeighbor ts edge

isNeighbor :: (Direction, Row) -> Tile -> Maybe (Tile, Direction)
isNeighbor (edgeDirection, edge) t =
  let tileConfigurations = take 4 (iterate rotateUp t) >>= \t' -> t':[flipTile t']
      matchTile = find (\t' -> edge == (snd . head) (getEdges t' [edgeDirection])) tileConfigurations
   in (\t' -> (t', edgeDirection)) <$> matchTile

updateTileDb :: TileId -> TileDb -> [(Tile, Direction)] -> TileDb
updateTileDb _ tDb [] = tDb
updateTileDb tId tDb ((n, nd):ns) =
  let tDb' = M.insert (tId, nd) n tDb
      tDb'' = if isJust (M.lookup (tileId n, Self) tDb')
        then tDb'
        else M.insert (tileId n, Self) n tDb'
  in updateTileDb tId tDb'' ns

getCorners :: TileDb -> [Tile]
getCorners tDb =
  let cornersId = (fmap head . filter ((4==) . length) . group . sort . M.foldrWithKey (\(tId,_) (Tile tId' _) acc -> tId:tId':acc) []) tDb
  in (\tId -> fromJust (M.lookup (tId, Self) tDb) ) <$> cornersId

buildImage :: TileDb -> Tile
buildImage tdb = undefined

buildTileDatabase :: [Tile] -> TileDb -> TileDb
buildTileDatabase [] tDb = tDb
buildTileDatabase (t:ts) tDb =
  let
    maybeTileSelf = M.lookup (tileId t, Self) tDb
    tileSelf = foldr const t maybeTileSelf
    openEdges = getEdges tileSelf $ howManyOpenEdges tileSelf tDb
    neighboors =
        catMaybes $
        foldl (\acc edge -> acc ++ [findNeighbor ts edge]) [] openEdges
    newTDb = updateTileDb (tileId t) tDb neighboors
    newTDb' = if isJust maybeTileSelf
      then newTDb
      else M.insert (tileId tileSelf, Self) tileSelf newTDb
   in buildTileDatabase ts newTDb'

seaMonster :: Tile
seaMonster = undefined

searchSeaMonster :: Tile -> [(Tile, Int)]
searchSeaMonster image = undefined

killSeaMonsters :: Tile -> Tile
killSeaMonsters image = undefined

countRoughWater :: Tile -> Int
countRoughWater t = undefined

solution2 :: [Tile] -> Int
solution2 ts =
  let tileDatabase = buildTileDatabase ts M.empty
      image = buildImage tileDatabase
      mostSeaMonsterConfiguration =
        fst . maximumBy (\(_, m) (_, m') -> m `compare` m') $
        searchSeaMonster image
      waters = killSeaMonsters mostSeaMonsterConfiguration
   in countRoughWater waters

-- test datat ------------------------------------------------------------
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
