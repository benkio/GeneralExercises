-------------------------------------------------------------------------------
--                           Advent Of Code - day 24                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentyFourthDecember where

import Data.Set as S (Set, empty, foldl, fromList, insert, null, size, union)

import Data.Either (isLeft)
import Data.List (group, sort)
import Prelude hiding (lookup)

data Direction
  = E
  | SE
  | SW
  | W
  | NW
  | NE
  deriving (Show, Eq, Ord)

type Coordinate = (Int, Int)

input :: IO [[Direction]]
input = fmap parseLine . lines <$> readFile "input/2020/24December.txt"

parseLine :: String -> [Direction]
parseLine [] = []
parseLine ('s':'w':xs) = SW : parseLine xs
parseLine ('s':'e':xs) = SE : parseLine xs
parseLine ('n':'w':xs) = NW : parseLine xs
parseLine ('n':'e':xs) = NE : parseLine xs
parseLine ('w':xs) = W : parseLine xs
parseLine ('e':xs) = E : parseLine xs

calculateTerminalCoordinate :: [Direction] -> Coordinate
calculateTerminalCoordinate = Prelude.foldl directionToCoordinate (0, 0)

directionToCoordinate :: Coordinate -> Direction -> Coordinate
directionToCoordinate (x, y) SW = (x - 1, y - 1)
directionToCoordinate (x, y) NE = (x + 1, y + 1)
directionToCoordinate (x, y) SE = (x, y - 1)
directionToCoordinate (x, y) NW = (x, y + 1)
directionToCoordinate (x, y) W = (x - 1, y)
directionToCoordinate (x, y) E = (x + 1, y)

solution1 :: [[Direction]] -> Int
solution1 directions =
  (length . filter ((0 /=) . (`mod` 2) . length) . group . sort)
    (calculateTerminalCoordinate <$> directions)

initialBlackTiles :: [[Direction]] -> [Coordinate]
initialBlackTiles ds =
  (fmap head . filter ((0 /=) . (`mod` 2) . length) . group . sort)
    (calculateTerminalCoordinate <$> ds)

getNeighborTiles :: Coordinate -> [Coordinate]
getNeighborTiles (x, y) =
  [ (x - 1, y - 1)
  , (x + 1, y + 1)
  , (x, y - 1)
  , (x, y + 1)
  , (x - 1, y)
  , (x + 1, y)
  ]

initialAllTilesToCompute :: [[Direction]] -> [Coordinate]
initialAllTilesToCompute ds =
  (concatMap (\x -> x : getNeighborTiles x) . fmap head . group . sort)
    (calculateTerminalCoordinate <$> ds)

computeTile :: S.Set Coordinate -> Coordinate -> Either Coordinate Coordinate
computeTile blacks c
  | c `elem` blacks &&
      (S.null adjacentBlackTiles || S.size adjacentBlackTiles > 2) = Right c
  | c `notElem` blacks && S.size adjacentBlackTiles == 2 = Left c
  -- keep same color
  | c `elem` blacks = Left c
  | c `notElem` blacks = Right c
  where
    adjacentBlackTiles :: S.Set Coordinate
    adjacentBlackTiles =
      (S.fromList . filter (`elem` blacks) . getNeighborTiles) c

dayComputation :: S.Set Coordinate -> S.Set Coordinate -> S.Set Coordinate
dayComputation blacks =
  S.foldl
    (\result x ->
       if isLeft (computeTile blacks x)
         then S.insert x result
         else result)
    S.empty

solution2 :: Int -> S.Set Coordinate -> S.Set Coordinate -> S.Set Coordinate
solution2 0 blacks _ = blacks
solution2 iterations blacks allTiles =
  let newBlacks = dayComputation blacks allTiles
   in solution2
        (iterations - 1)
        newBlacks
        (S.foldl
           (\acc x -> acc `S.union` S.fromList (x : getNeighborTiles x))
           S.empty
           newBlacks)

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = do
  i <- input
  let blacks = S.fromList $ initialBlackTiles i
      allTiles = S.fromList $ initialAllTilesToCompute i
  return $ length $ solution2 100 blacks allTiles

testSolution2 :: Int -> Int
testSolution2 iterations =
  let blacks = S.fromList $ initialBlackTiles inputTest
      allTiles = S.fromList $ initialAllTilesToCompute inputTest
   in length $ solution2 iterations blacks allTiles

inputTest :: [[Direction]]
inputTest =
  (fmap parseLine . lines)
    "sesenwnenenewseeswwswswwnenewsewsw\n\
\neeenesenwnwwswnenewnwwsewnenwseswesw\n\
\seswneswswsenwwnwse\n\
\nwnwneseeswswnenewneswwnewseswneseene\n\
\swweswneswnenwsewnwneneseenw\n\
\eesenwseswswnenwswnwnwsewwnwsene\n\
\sewnenenenesenwsewnenwwwse\n\
\wenwwweseeeweswwwnwwe\n\
\wsweesenenewnwwnwsenewsenwwsesesenwne\n\
\neeswseenwwswnwswswnw\n\
\nenwswwsewswnenenewsenwsenwnesesenew\n\
\enewnwewneswsewnwswenweswnenwsenwsw\n\
\sweneswneswneneenwnewenewwneswswnese\n\
\swwesenesewenwneswnwwneseswwne\n\
\enesenwswwswneneswsenwnewswseenwsese\n\
\wnwnesenesenenwwnenwsewesewsesesew\n\
\nenewswnwewswnenesenwnesewesw\n\
\eneswnwswnwsenenwnwnwwseeswneewsenese\n\
\neswnwewnwnwseenwseesewsenwsweewe\n\
\wseweeenwnesenwwwswnew"
