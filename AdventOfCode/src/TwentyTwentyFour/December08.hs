module TwentyTwentyFour.December08 where

-- import Data.Map (Map, fromList)

import Data.Bifunctor (bimap)
import Data.List (maximum, tails)
import Data.Set (fromList, toList)

type Coord = (Int, Int)
type MapContent = Char
type CityMap = [(Coord, MapContent)] -- Map Coord MapContent

input :: IO CityMap
input = parseInput <$> readFile "input/2024/December08.txt"

parseInput :: String -> CityMap
parseInput =
    concatMap
        ( \(y, s) ->
            ( fmap (\(x, c) -> ((x, y), c))
                . zip [0 ..]
            )
                s
        )
        . zip [0 ..]
        . lines

testInput :: CityMap
testInput =
    parseInput
        "............\n\
        \........0...\n\
        \.....0......\n\
        \.......0....\n\
        \....0.......\n\
        \......A.....\n\
        \............\n\
        \............\n\
        \........A...\n\
        \.........A..\n\
        \............\n\
        \............"

cityMax :: CityMap -> Coord
cityMax cm = bimap maximum maximum (fmap fst f, fmap snd f)
  where
    f = fmap fst cm

findAntinodes :: (Int, Int) -> ((Int, Int), MapContent) -> ((Int, Int), MapContent) -> [Coord]
findAntinodes _ (_, '.') (_, _) = []
findAntinodes _ (_, _) (_, '.') = []
findAntinodes (maxX, maxY) ((x, y), a) ((x', y'), a')
    | a == a' =
        filter
            (\(a, b) -> a >= 0 && b >= 0 && a <= maxX && b <= maxY)
            [ (x + (x - x'), y + (y - y'))
            , (x' + (x' - x), y' + (y' - y))
            ]
    | otherwise = []

isAntenna :: ((Int, Int), MapContent) -> Bool
isAntenna (_, '.') = False
isAntenna (_, _) = True

solution1 :: CityMap -> Int
solution1 as =
    length . uniquify $
        [(x, y) | (x : ys) <- Data.List.tails as, y <- ys] >>= uncurry (findAntinodes maxP)
  where
    maxP = cityMax as
    antennas = filter isAntenna as

uniquify lst = toList $ fromList lst

december08Solution1 :: IO Int
december08Solution1 = solution1 <$> input

findAntinodes' :: ((Int, Int), MapContent) -> ((Int, Int), MapContent) -> [Coord]
findAntinodes' (_, '.') (_, _) = []
findAntinodes' (_, _) (_, '.') = []
findAntinodes' ((x, y), a) ((x', y'), a')
    | a == a' =
        let
            p = (x + (x - x'), y + (y - y'))
            p' = (x' + (x' - x), y' + (y' - y))
         in
            [(x, y), (x', y')] ++ go p p'
    | otherwise = []
  where
    go (c, d) (e, f) =
        let
            q = (c + (x - x'), d + (y - y'))
            q' = (e + (x' - x), f + (y' - y))
         in
            [(c, d), (e, f)] ++ go q q'

solution2 :: CityMap -> Int
solution2 as =
    length . uniquify . filter (\(a, b) -> a >= 0 && b >= 0 && a <= maxX && b <= maxY) $
        [(x, y) | (x : ys) <- Data.List.tails as, y <- ys] >>= take (maxX * maxY) . uncurry findAntinodes'
  where
    (maxX, maxY) = cityMax as
    antennas = filter isAntenna as

december08Solution2 :: IO Int
december08Solution2 = solution2 <$> input
