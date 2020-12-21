-------------------------------------------------------------------------------
--                           Advent Of Code - day 20                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentiethDecember where

import           Data.Bifunctor (second)
import           Data.List      (delete, find, group, groupBy, partition, sort,
                                 transpose)
import           Data.Map       (Map, empty, insert, mapKeys, toList, union)
import           Data.Maybe     (fromJust, isJust)

type Coordinate = (Int, Int)

data Tile =
  Tile Int (Map Coordinate Bool)
  deriving (Show)

tileId :: Tile -> Int
tileId (Tile i _) = i

tileContent :: Tile -> (Map Coordinate Bool)
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
  (fmap (foldl1 (\(Tile x m1) (Tile _ m2) -> Tile x (m1 `union` m2))) .
   groupBy (\t1 t2 -> tileId t1 == tileId t2) .
   fmap
     (\(y, s') ->
        Tile
          tId
          (foldl
             (\m (x, c) ->
                if c == '#'
                  then insert (x, y) True m
                  else insert (x, y) False m)
             empty
             s')) .
   fmap (second (zip [0 ..])) . zip [0 ..] . tail)
    rawTile

tileEdges :: (Map Coordinate Bool) -> [[(Coordinate, Bool)]]
tileEdges t1 =
  let (t1EdgesWest, t1EdgesEst) =
        ((\l -> (take 10 l, drop (length l - 10) l)) . toList) t1
      (t1EdgesSouth, t1EdgesNorth) =
        ((\l -> (fmap (l !!) [9,19 .. 99], fmap (l !!) [0,10 .. 90])) . toList)
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
removeDuplicateEdges (edgesToCheck, standaloneEdges) (tId, edges) =
  let (matches, notMatches) =
        partition
          (\x -> fmap snd x `elem` (edgesToCheck >>= (snd . stripCoordinates)))
          edges
      edgesToCheckNoDuplicates =
        second
          (filter
             (\x ->
                fmap snd x `notElem`
                (fmap . fmap) snd (matches ++ fmap reverse matches))) <$>
        edgesToCheck
   in (edgesToCheckNoDuplicates, standaloneEdges ++ [(tId, notMatches)])

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
   transpose . groupBy (\((x, _), _) ((x', _), _) -> x == x') . toList)
    content

solution1 :: String -> Int
solution1 = product . fmap fst . cornerTiles

cornerTiles :: String -> [(Int, Coordinate)]
cornerTiles s =
  let tiles = parseTiles s
      edges =
        addReverseEdges . (\t -> (tileId t, tileEdges (tileContent t))) <$>
        tiles
      corners =
        (fmap (second findCorner) .
         filter ((2 <) . length . snd) . (`removeDuplicatesEdgesInTailes` []))
          edges
   in corners

findCorner :: [[(Coordinate, Bool)]] -> Coordinate
findCorner xs =
  (fst . head . head . filter ((== length xs) . length) . group . sort . concat)
    xs

adjustNewOrigin :: Map Coordinate Bool -> Coordinate -> Map Coordinate Bool
adjustNewOrigin content (x, y)
  | x == 0 && y == 0 = content
  | otherwise = mapKeys (\(x', y') -> (abs (x' - x), abs (y' - y))) content

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

solution2 s =
  let tiles = parseTiles s
      bigMosaic =
        linkTiles
          ((fmap tileContent . tail) tiles)
          ((tileContent . head) tiles)
          ((tileEdges . tileContent . head) tiles)
   in showTile $ Tile 0 bigMosaic

linkTiles ::
     [Map Coordinate Bool]
  -> Map Coordinate Bool
  -> [[(Coordinate, Bool)]]
  -> Map Coordinate Bool
linkTiles [] result _ = result
linkTiles (t:ts) result resultEdges =
  let maybeMatchResultNEdges = searchMatch resultEdges t
   in foldr
        (\(tileToAdd, matchingEdge) _ ->
           let result' = mergeTile result tileToAdd matchingEdge
               resultEdges' =
                 delete matchingEdge resultEdges ++
                 delete matchingEdge (tileEdges t)
            in linkTiles ts result' resultEdges')
        (linkTiles (ts ++ [t]) result resultEdges)
        maybeMatchResultNEdges

searchMatch ::
     [[(Coordinate, Bool)]]
  -> Map Coordinate Bool
  -> Maybe (Map Coordinate Bool, [(Coordinate, Bool)])
searchMatch resultEdges t =
  let tEdges = tileEdges t
   in foldl
        (\acc x ->
           if isJust acc
             then acc
             else matchEdge x resultEdges t)
        Nothing
        tEdges

-- The tile rotates the big result doesn't move
matchEdge ::
     [(Coordinate, Bool)]
  -> [[(Coordinate, Bool)]]
  -> Map Coordinate Bool
  -> Maybe (Map Coordinate Bool, [(Coordinate, Bool)])
matchEdge edge resultEdges t
  | fmap snd edge `elem` (fmap . fmap) snd resultEdges = Just (t, edge)
  | (reverse . fmap snd) edge `elem` (fmap . fmap) snd resultEdges =
    Just (adjustNewOrigin t (maximum (fmap fst edge)), edge)
  | otherwise = Nothing

mergeTile ::
     Map Coordinate Bool
  -> Map Coordinate Bool
  -> [(Coordinate, Bool)]
  -> Map Coordinate Bool
mergeTile result tileToAdd commonEdge = undefined

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
