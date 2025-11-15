{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December15b where

import Data.Bifunctor (bimap, first, second)
import Data.Functor ((<&>))
import Data.List (find, nub, sortOn)
import Data.Map (Map, fromList, keys, (!?))
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, pack, splitOn, unpack)
import Debug.Trace
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

data SeaObj = BoxL | BoxR | Wall deriving (Show, Eq)

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
lookupByMove :: Move -> Coord -> SeaMap -> Either (Maybe (Coord, SeaObj)) Coord
lookupByMove m c sm =
    maybe
        (Right c')
        (Left . (\so -> if so == Wall then Nothing else Just (c', so)))
        (sm !? c')
  where
    c' = coordMove m c

otherBox :: SeaObj -> SeaObj
otherBox BoxL = BoxR
otherBox BoxR = BoxL

collectSeaObjMoveHorizontal :: Move -> (Coord, SeaObj) -> SeaMap -> Maybe [((Coord, Coord), SeaObj)]
collectSeaObjMoveHorizontal m (c, o) sm = case lookupByMove m c sm of
    Right c' -> Just [((c, c'), o)]
    Left Nothing -> Nothing
    Left (Just (c', o')) -> (((c, c'), o) :) <$> collectSeaObjMoveHorizontal m (c', o') sm

underBoxCoords :: (Coord, SeaObj) -> ((Coord, SeaObj), (Coord, SeaObj))
underBoxCoords (c, BoxL) = ((c, BoxL), (first (+ 1) c, BoxR))
underBoxCoords (c, BoxR) = ((first (\x -> x - 1) c, BoxL), (c, BoxR))

uniqueFromLast :: [((Coord, Coord), SeaObj)] -> [((Coord, Coord), SeaObj)]
uniqueFromLast = reverse . nub . reverse

collectSeaObjMoveVertical :: Move -> (Coord, SeaObj) -> SeaMap -> Maybe [((Coord, Coord), SeaObj)]
collectSeaObjMoveVertical m (c, o) sm = case (lookupByMove m cl sm, lookupByMove m cr sm) of
    (Left Nothing, _) -> Nothing
    (_, Left Nothing) -> Nothing
    (Right ecl, Right ecr) -> Just [((cl, ecl), ol), ((cr, ecr), or)]
    (Left (Just (cl', ol')), Left (Just (cr', or'))) ->
        ( \xs ->
            collectSeaObjMoveVertical m (cr', or') sm
                >>= ( \xs ->
                        uniqueFromLast . (xs ++)
                            <$> collectSeaObjMoveVertical m (cl', ol') sm
                    )
                    . (xs ++)
        )
            [((cl, cl'), ol), ((cr, cr'), or)]
    (Right ecl, Left (Just (cr', or'))) ->
        ( \xs ->
            uniqueFromLast . (xs ++)
                <$> collectSeaObjMoveVertical m (cr', or') sm
        )
            [((cl, ecl), ol), ((cr, cr'), or)]
    (Left (Just (cl', ol')), Right ecr) ->
        ( \xs ->
            uniqueFromLast . (xs ++)
                <$> collectSeaObjMoveVertical m (cl', ol') sm
        )
            [((cl, cl'), ol), ((cr, ecr), or)]
  where
    ((cl, ol), (cr, or)) = underBoxCoords (c, o)

collectSeaObjMove :: Move -> (Coord, SeaObj) -> SeaMap -> Maybe [((Coord, Coord), SeaObj)]
collectSeaObjMove R = collectSeaObjMoveHorizontal R
collectSeaObjMove L = collectSeaObjMoveHorizontal L
collectSeaObjMove U = collectSeaObjMoveVertical U
collectSeaObjMove D = collectSeaObjMoveVertical D

moveSingle :: Sea -> Sea
moveSingle s@S{sMap = sm, sRobot = Robot{rCoord = rc}, sMoves = (m : ms)} =
    case robotMove of
        Right c' -> s{sRobot = Robot{rCoord = c'}, sMoves = ms}
        Left Nothing -> s{sMoves = ms}
        Left (Just (c', o)) ->
            maybe
                (s{sMoves = ms})
                ( \updates ->
                    s
                        { sRobot = Robot{rCoord = c'}
                        , sMap = updateKeys sm (reverse updates)
                        , sMoves = ms
                        }
                )
                $ collectSeaObjMove m (c', o) sm
  where
    robotMove = lookupByMove m rc sm

move :: Sea -> Sea
move =
    until (null . sMoves) moveSingle

calculateGPS :: Sea -> Int
calculateGPS = sum . fmap (uncurry (+) . second (* 100) . manhattanDistance (0, 0)) . keys . M.filter (== BoxL) . sMap

solution2 :: Sea -> Int
solution2 = calculateGPS . move

-- too high 1435179
--          1425169
december15Solution2 :: IO Int
december15Solution2 = solution2 <$> input

testInput :: Sea
testInput =
    parseInput
        "#######\n\
        \#...#.#\n\
        \#.....#\n\
        \#..OO@#\n\
        \#..O..#\n\
        \#.....#\n\
        \#######\n\
        \\n\
        \<vv<<^^<<^^\n"

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

testInput3 :: Sea
testInput3 =
    parseInput
        "#########\n\
        \#.....###\n\
        \#...O...#\n\
        \#...OO@.#\n\
        \#...O...#\n\
        \##......#\n\
        \#.......#\n\
        \#########\n\
        \\n\
        \<vv<<^\n"

parseInput :: Text -> Sea
parseInput =
    ( \[s, m] ->
        S
            { sMap = (parseMap . unpack) s
            , sRobot = Robot{rCoord = (parseRobot . unpack) s}
            , sMoves = parseMoves m
            }
    )
        . splitOn "\n\n"
  where
    parseMap :: String -> SeaMap
    parseMap =
        fromList
            . fmap (second fromJust)
            . concatMap (filter (isJust . snd) . (\(y, s) -> expandMap y 0 s))
            . zip [0 ..]
            . lines
    parseRobot :: String -> Coord
    parseRobot =
        first (* 2)
            . fst
            . fromJust
            . find ((== '@') . snd)
            . concatMap
                ( \(y, s) ->
                    zipWith (\x c -> ((x, y), c)) [0 ..] s
                )
            . zip [0 ..]
            . lines
    expandMap :: Int -> Int -> String -> [(Coord, Maybe SeaObj)]
    expandMap y x ('#' : xs) = [((x, y), Just Wall), ((x + 1, y), Just Wall)] ++ expandMap y (x + 2) xs
    expandMap y x ('O' : xs) = [((x, y), Just BoxL), ((x + 1, y), Just BoxR)] ++ expandMap y (x + 2) xs
    expandMap y x ('.' : xs) = [((x, y), Nothing), ((x + 1, y), Nothing)] ++ expandMap y (x + 2) xs
    expandMap y x ('@' : xs) = [((x, y), Nothing), ((x + 1, y), Nothing)] ++ expandMap y (x + 2) xs
    expandMap _ _ [] = []
    parseMoves :: Text -> [Move]
    parseMoves = fmap parseMove . filter (/= '\n') . unpack

printSeaMap :: Sea -> Move -> IO ()
printSeaMap S{sMap = sm, sRobot = Robot{rCoord = rc}} m =
    putStrLn . printGridMap printSeaObj $ sm
  where
    printSeaObj _ (Just Wall) = "#"
    printSeaObj _ (Just BoxL) = "["
    printSeaObj _ (Just BoxR) = "]"
    printSeaObj c Nothing = if c == rc then [printMove m] else "."

testMove :: Sea -> IO ()
testMove =
    (`printSeaMap` U) . move

testMoveAt :: Int -> Sea -> IO ()
testMoveAt n =
    (`printSeaMap` U) . getSeaAtTime n

getSeaAtTime :: Int -> Sea -> Sea
getSeaAtTime n = (!! n) . iterate moveSingle

shit = fst . until (uncurry iFindTheShit) (bimap (+ 1) moveSingle) . (0,)
  where
    iFindTheShit _ = (\xs -> (BoxL, BoxL) `elem` zip xs (tail xs)) . fmap snd . sortOn (snd . fst) . M.toList . sMap

beforeCrash = getSeaAtTime 5 testInput3
