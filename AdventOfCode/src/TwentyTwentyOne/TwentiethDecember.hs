module TwentyTwentyOne.TwentiethDecember where

import Control.Monad (mapM_)
import Data.Bifunctor (second)
import Data.List (intersperse, sortBy)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M (empty, foldrWithKey, fromList, insert, keys, lookup)

type Coord = (Int, Int)

parseInput :: String -> (String, Map Coord Char)
parseInput = (\xs -> (head xs, buildGrid (tail xs))) . filter (not . null) . lines

parseGrid :: Char -> [String] -> Map Coord Char
parseGrid def xs = buildGrid $ fmap (\as -> def : as ++ [def]) ([emptyLine] ++ xs ++ [emptyLine])
  where
    emptyLine = replicate (length (head xs)) def

buildGrid ys = M.fromList $ concatMap (\(y, zs) -> fmap (\(x, v) -> ((x, y), v)) ([0 ..] `zip` zs)) $ [0 ..] `zip` ys

stringToInt :: String -> Int
stringToInt = foldl (\acc (i, c) -> acc + (if c == '#' then 2 ^ i else 0)) 0 . zip [0 ..] . reverse

subImageToString :: Char -> Map Coord Char -> [Coord] -> String
subImageToString def m cs = (\(x, y) -> foldl (\_ v -> v) def (M.lookup (x, y) m)) <$> cs

convertImage :: Map (Int, Int) Char -> ([String] -> a) -> a
convertImage m f =
  let maxX = (maximum . fmap fst . M.keys) m
      maxY = (maximum . fmap snd . M.keys) m
      grid = [foldl (\_ v -> v) '.' (M.lookup (x, y) m) | y <- [0 .. maxY], x <- [0 .. maxX]]
      gridByLine = chunksOf (maxX + 1) grid
   in f gridByLine

printImage m = convertImage m (mapM_ putStrLn)

mapToString m = convertImage m (concat . intersperse "\n")

neighboorsCoords :: Coord -> [Coord]
neighboorsCoords (x, y) =
  sortBy (\(_, y) (_, y') -> y `compare` y') $ [(a, b) | a <- [(x -1) .. (x + 1)], b <- [(y -1) .. (y + 1)]]

computeInfiniteDefault :: Char -> String -> Char
computeInfiniteDefault '.' mapping = head mapping
computeInfiniteDefault '#' mapping = last mapping

imageAlgorithm :: Char -> String -> [String] -> (String, Char)
imageAlgorithm def mapping =
  (\s -> (s, computeInfiniteDefault def mapping))
    . ( \m ->
          mapToString
            ( M.foldrWithKey
                ( \c _ m' ->
                    M.insert c (mapping !! stringToInt (subImageToString def m (neighboorsCoords c))) m'
                )
                M.empty
                m
            )
      )
    . parseGrid def

performNSteps :: Int -> String -> String -> String
performNSteps i m s = fst $ iterate (\(x, def) -> imageAlgorithm def m (lines x)) (s, '.') !! i

solution :: Int -> String -> Int
solution c =
  length
    . (\(m, image) -> filter (== '#') $ performNSteps c m image)
    . second mapToString
    . parseInput

input :: IO String
input = readFile "input/2021/20December.txt"

inputTest :: String
inputTest =
  "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\
  \\n\
  \#..#.\n\
  \#....\n\
  \##..#\n\
  \..#..\n\
  \..###"

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution 2 <$> input

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution 50 <$> input
