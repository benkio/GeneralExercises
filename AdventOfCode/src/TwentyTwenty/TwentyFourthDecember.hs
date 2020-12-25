-------------------------------------------------------------------------------
--                           Advent Of Code - day 24                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentyFourthDecember where

import           Data.List (group, sort)
import           Prelude   hiding (lookup)

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
parseLine []           = []
parseLine ('s':'w':xs) = SW : parseLine xs
parseLine ('s':'e':xs) = SE : parseLine xs
parseLine ('n':'w':xs) = NW : parseLine xs
parseLine ('n':'e':xs) = NE : parseLine xs
parseLine ('w':xs)     = W : parseLine xs
parseLine ('e':xs)     = E : parseLine xs

calculateTerminalCoordinate :: [Direction] -> Coordinate
calculateTerminalCoordinate = foldl directionToCoordinate (0, 0)

directionToCoordinate :: Coordinate -> Direction -> Coordinate
directionToCoordinate (x, y) SW = (x - 1, y - 1)
directionToCoordinate (x, y) NE = (x + 1, y + 1)
directionToCoordinate (x, y) SE = (x, y - 1)
directionToCoordinate (x, y) NW = (x, y + 1)
directionToCoordinate (x, y) W  = (x - 1, y)
directionToCoordinate (x, y) E  = (x + 1, y)

solution1 :: [[Direction]] -> Int
solution1 directions =
  (length . filter ((0 /=) . (`mod` 2) . length) . group . sort)
    (calculateTerminalCoordinate <$> directions)

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
