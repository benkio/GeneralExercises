{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyFour.December15a where

import Data.Bifunctor (first, second)
import Data.Functor ((<&>))
import Data.Map (Map, fromList, keys, (!?))
import qualified Data.Map as M (filter)
import Data.Text (Text, pack, splitOn, unpack)
import Lib.Coord (Coord, manhattanDistance)
import Lib.CoordMove (coordMove)
import Lib.Map (updateKeys)
import Lib.Move (Move (..))
import Lib.Parse (parseGridWithElemSelection, parseMove)
import Lib.Print (printGridMap, printMove)

data Sea = S
    { sMap :: SeaMap
    , sRobot :: Robot
    , sMoves :: [Move]
    }
    deriving (Show)

data SeaObj = Box | Wall deriving (Show, Eq)

type SeaMap = Map Coord SeaObj

newtype Robot = Robot {rCoord :: Coord} deriving (Show)

input :: IO Sea
input = parseInput . pack <$> readFile "input/2024/December15.txt"

{-
  Right: empty space Coord
  Left: SeaObj
    - Nothing: Wall
    - Just: Box Coord
-}
lookupByMove :: Move -> Coord -> SeaMap -> Either (Maybe Coord) Coord
lookupByMove m c sm =
    maybe
        (Right c')
        (Left . (\so -> if so == Wall then Nothing else Just c'))
        (sm !? c')
  where
    c' = coordMove m c

collectSeaObjMove :: Move -> (Coord, SeaObj) -> SeaMap -> Maybe [((Coord, Coord), SeaObj)]
collectSeaObjMove m (c, o) sm = case mso of
    Right c' -> Just [((c, c'), o)]
    Left Nothing -> Nothing
    Left (Just c') -> (((c, c'), o) :) <$> collectSeaObjMove m (c', Box) sm
  where
    mso = lookupByMove m c sm

moveSingle :: Sea -> Sea
moveSingle s@S{sMap = sm, sRobot = Robot{rCoord = rc}, sMoves = (m : ms)} =
    case robotMove of
        Right c' -> s{sRobot = Robot{rCoord = c'}, sMoves = ms}
        Left Nothing -> s{sMoves = ms}
        Left (Just c') ->
            maybe
                (s{sMoves = ms})
                ( \updates ->
                    s
                        { sRobot = Robot{rCoord = c'}
                        , sMap = updateKeys sm (reverse updates)
                        , sMoves = ms
                        }
                )
                $ collectSeaObjMove m (c', Box) sm
  where
    robotMove = lookupByMove m rc sm

move :: Sea -> Sea
move =
    until (null . sMoves) moveSingle

calculateGPS :: Sea -> Int
calculateGPS = sum . fmap (uncurry (+) . second (* 100) . manhattanDistance (0, 0)) . keys . M.filter (== Box) . sMap

solution1 :: Sea -> Int
solution1 = calculateGPS . move

december15Solution1 :: IO Int
december15Solution1 = solution1 <$> input

testInput2 :: Sea
testInput2 =
    parseInput
        "########\n\
        \#..O.O.#\n\
        \##@.O..#\n\
        \#...O..#\n\
        \#.#.O..#\n\
        \#...O..#\n\
        \#......#\n\
        \########\n\
        \\n\
        \<^^>>>vv<v>>v<<\n"

testInput1 :: Sea
testInput1 =
    parseInput
        "##########\n\
        \#..O..O.O#\n\
        \#......O.#\n\
        \#.OO..O.O#\n\
        \#..O@..O.#\n\
        \#O#..O...#\n\
        \#O..O..O.#\n\
        \#.OO.O.OO#\n\
        \#....O...#\n\
        \##########\n\
        \\n\
        \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
        \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
        \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
        \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
        \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
        \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
        \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
        \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
        \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
        \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n"

parseInput :: Text -> Sea
parseInput =
    ( \[s, m] ->
        let (seaMap, robotCoord) = first fromList $ parseGridWithElemSelection parseSeaObj (unpack s)
            moves = parseMoves m
         in S{sMap = seaMap, sRobot = Robot{rCoord = head robotCoord}, sMoves = moves}
    )
        . splitOn "\n\n"
  where
    parseSeaObj :: Int -> Int -> Char -> Maybe (Either (Coord, SeaObj) Coord)
    parseSeaObj y x '#' = Just $ Left ((x, y), Wall)
    parseSeaObj y x 'O' = Just $ Left ((x, y), Box)
    parseSeaObj y x '@' = Just $ Right (x, y)
    parseSeaObj y x _ = Nothing
    parseMoves :: Text -> [Move]
    parseMoves = fmap parseMove . filter (/= '\n') . unpack

printSeaMap :: Sea -> Move -> IO ()
printSeaMap S{sMap = sm, sRobot = Robot{rCoord = rc}} m =
    putStrLn . printGridMap printSeaObj $ sm
  where
    printSeaObj _ (Just Wall) = "#"
    printSeaObj _ (Just Box) = "O"
    printSeaObj c Nothing = if c == rc then [printMove m] else "."

testMove :: Sea -> IO ()
testMove =
    (`printSeaMap` U) . move

getSeaAtTime :: Int -> Sea -> Sea
getSeaAtTime n = (!! n) . iterate moveSingle

testMoveAt :: Int -> Sea -> IO ()
testMoveAt n =
    (`printSeaMap` U) . getSeaAtTime n
