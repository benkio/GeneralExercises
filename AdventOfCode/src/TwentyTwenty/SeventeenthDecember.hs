{-# LANGUAGE TupleSections #-}

-------------------------------------------------------------------------------
--                           Advent Of Code - day 17                          --
-------------------------------------------------------------------------------
module TwentyTwenty.SeventeenthDecember where

import Data.List (groupBy, sortBy, transpose)
import qualified Data.Map as M (
    Map,
    difference,
    filter,
    filterWithKey,
    fromList,
    keys,
    lookup,
    mapWithKey,
    size,
    toList,
    union,
 )

type Coordinate = (Int, Int, Int)

type Grid = M.Map Coordinate Bool

compareCondinates :: Coordinate -> Coordinate -> Ordering
compareCondinates (x, y, z) (x', y', z')
    | z' > z || (z' == z && y' > y) || (z' == z && y' == y && x' > x) = GT
    | x == x' && y == y' && z == z' = EQ
    | otherwise = LT

input :: IO Grid
input = initialGrid <$> readFile "input/2020/17December.txt"

test :: String
test =
    ".#.\n\
    \..#\n\
    \###"

initialGrid :: String -> Grid
initialGrid = M.fromList . foldl lineToCubes [] . zip [0 ..] . lines
  where
    lineToCubes :: [(Coordinate, Bool)] -> (Int, String) -> [(Coordinate, Bool)]
    lineToCubes acc (y, s) =
        let xCubes =
                foldl
                    ( \acc' (x, c) ->
                        if c == '#'
                            then acc' ++ [((x, y, 0), True)]
                            else acc' ++ [((x, y, 0), False)]
                    )
                    []
                    (zip [0 ..] s)
         in acc ++ xCubes

showGrid :: Grid -> String
showGrid =
    snd
        . foldl collapseGrid ((0, 0), "")
        . sortBy (flip (\(c, _) (c', _) -> compareCondinates c c'))
        . M.toList
  where
    collapseGrid ::
        ((Int, Int), String) -> (Coordinate, Bool) -> ((Int, Int), String)
    collapseGrid ((y', z'), acc) ((_, y, z), s) =
        let (z'', acc') =
                if z /= z'
                    then (z, acc ++ "\n z=" ++ show z ++ " \n")
                    else (z', acc)
            (y'', acc'') =
                if y /= y'
                    then (y, acc' ++ "\n")
                    else (y', acc')
         in ( (y'', z'')
            , acc''
                ++ ( if s
                        then "#"
                        else "."
                   )
            )

neighborCoordinates :: Coordinate -> [Coordinate]
neighborCoordinates (x, y, z) =
    [ (a, b, c)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , a /= x || b /= y || c /= z
    ]

findNeighbors :: Coordinate -> Grid -> Grid
findNeighbors c g =
    ( M.fromList
        . fmap (\x -> maybe (x, False) (x,) (M.lookup x g))
        . neighborCoordinates
    )
        c

neighborToStatus :: Bool -> Grid -> Bool
neighborToStatus status neighboor
    | status && (activeNeighboor == 2 || activeNeighboor == 3) = True
    | not status && activeNeighboor == 3 = True
    | otherwise = False
  where
    activeNeighboor = M.size (M.filter id neighboor)

enlargeGrid :: Grid -> Grid
enlargeGrid g =
    let ((lx, ly, lz), (hx, hy, hz)) =
            (\ks -> (minimum ks, maximum ks)) (M.keys g)
        emptyGrid =
            M.fromList
                [ ((a, b, c), False)
                | a <- [lx - 1 .. hx + 1]
                , b <- [ly - 1 .. hy + 1]
                , c <- [lz - 1 .. hz + 1]
                ]
     in M.union g emptyGrid

shrinkGridIfExternalFalse :: Grid -> Grid
shrinkGridIfExternalFalse g =
    let ((lx, ly, lz), (hx, hy, hz)) =
            (\ks -> (minimum ks, maximum ks)) (M.keys g)
        internalGrid =
            M.filterWithKey
                ( \(x, y, z) _ ->
                    (x < hx && x > lx) && (y < hy && y > ly) && (z < hz && z > lz)
                )
                g
        externalGrid = M.difference g internalGrid
        externalGridNonEmpty = (any snd . M.toList) externalGrid
     in if externalGridNonEmpty
            then g
            else internalGrid

checkCube :: Grid -> (Coordinate, Bool) -> Bool
checkCube grid (coord, cube) = neighborToStatus cube (findNeighbors coord grid)

computeStep :: Grid -> Grid
computeStep g =
    let enlargedGrid = enlargeGrid g
     in (shrinkGridIfExternalFalse . M.mapWithKey (curry (checkCube enlargedGrid)))
            enlargedGrid

testSolution :: Int
testSolution =
    (M.size . M.filter id) $ iterate computeStep (initialGrid test) !! 6

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 =
    (M.size . M.filter id) . (!! 6) . iterate computeStep <$> input

-- Part2 ---------------------------------------
type Coordinate2 = (Int, Int, Int, Int)

type Grid2 = M.Map Coordinate2 Bool

input2 :: IO Grid2
input2 = initialGrid2 <$> readFile "input/2020/17December.txt"

initialGrid2 :: String -> Grid2
initialGrid2 = M.fromList . foldl lineToCubes [] . zip [0 ..] . lines
  where
    lineToCubes ::
        [(Coordinate2, Bool)] -> (Int, String) -> [(Coordinate2, Bool)]
    lineToCubes acc (y, s) =
        let xCubes =
                foldl
                    ( \acc' (x, c) ->
                        if c == '#'
                            then acc' ++ [((x, y, 0, 0), True)]
                            else acc' ++ [((x, y, 0, 0), False)]
                    )
                    []
                    (zip [0 ..] s)
         in acc ++ xCubes

neighborCoordinates2 :: Coordinate2 -> [Coordinate2]
neighborCoordinates2 (x, y, z, w) =
    [ (a, b, c, d)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , d <- [w - 1 .. w + 1]
    , a /= x || b /= y || c /= z || d /= w
    ]

findNeighbors2 :: Coordinate2 -> Grid2 -> Grid2
findNeighbors2 c g =
    ( M.fromList
        . fmap (\x -> maybe (x, False) (x,) (M.lookup x g))
        . neighborCoordinates2
    )
        c

neighborToStatus2 :: Bool -> Grid2 -> Bool
neighborToStatus2 status neighboor
    | status && (activeNeighboor == 2 || activeNeighboor == 3) = True
    | not status && activeNeighboor == 3 = True
    | otherwise = False
  where
    activeNeighboor = M.size (M.filter id neighboor)

enlargeGrid2 :: Grid2 -> Grid2
enlargeGrid2 g =
    let ((lx, ly, lz, lw), (hx, hy, hz, hw)) =
            (\ks -> (minimum ks, maximum ks)) (M.keys g)
        emptyGrid =
            M.fromList
                [ ((a, b, c, d), False)
                | a <- [lx - 1 .. hx + 1]
                , b <- [ly - 1 .. hy + 1]
                , c <- [lz - 1 .. hz + 1]
                , d <- [lw - 1 .. hw + 1]
                ]
     in M.union g emptyGrid

shrinkGridIfExternalFalse2 :: Grid2 -> Grid2
shrinkGridIfExternalFalse2 g =
    let ((lx, ly, lz, lw), (hx, hy, hz, hw)) =
            (\ks -> (minimum ks, maximum ks)) (M.keys g)
        internalGrid =
            M.filterWithKey
                ( \(x, y, z, w) _ ->
                    (x < hx && x > lx)
                        && (y < hy && y > ly)
                        && (z < hz && z > lz)
                        && (w < hw && w > lw)
                )
                g
        externalGrid = M.difference g internalGrid
        externalGridNonEmpty = (any snd . M.toList) externalGrid
     in if externalGridNonEmpty
            then g
            else internalGrid

checkCube2 :: Grid2 -> (Coordinate2, Bool) -> Bool
checkCube2 grid (coord, cube) =
    neighborToStatus2 cube (findNeighbors2 coord grid)

computeStep2 :: Grid2 -> Grid2
computeStep2 g =
    let enlargedGrid = enlargeGrid2 g
     in ( shrinkGridIfExternalFalse2
            . M.mapWithKey (curry (checkCube2 enlargedGrid))
        )
            enlargedGrid

testSolution2 :: Int
testSolution2 =
    (M.size . M.filter id) $ iterate computeStep2 (initialGrid2 test) !! 6

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 =
    (M.size . M.filter id) . (!! 6) . iterate computeStep2 <$> input2
