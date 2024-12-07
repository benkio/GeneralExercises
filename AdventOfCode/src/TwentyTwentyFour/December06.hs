module TwentyTwentyFour.December06 where

import Data.Bifunctor (bimap, first, second)
import Data.Either (isRight, partitionEithers, rights)
import Data.List (nub)
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)
data Direction = U | D | L | R
type Field = [Coord]

input :: IO (Field, Coord)
input = parseInput <$> readFile "input/2024/December06.txt"

parseInput :: String -> (Field, Coord)
parseInput = second head . partitionEithers . concatMap (\(y, s) -> mapMaybe (parseGuardOrObstacle y) (zip [0 ..] s)) . zip [0 ..] . lines
  where
    parseGuardOrObstacle :: Int -> (Int, Char) -> Maybe (Either Coord Coord)
    parseGuardOrObstacle _ (_, '.') = Nothing
    parseGuardOrObstacle y (x, '#') = Just $ Left (x, y)
    parseGuardOrObstacle y (x, '^') = Just $ Right (x, y)

testInput :: (Field, Coord)
testInput =
    parseInput
        "....#.....\n\
        \.........#\n\
        \..........\n\
        \..#.......\n\
        \.......#..\n\
        \..........\n\
        \.#..^.....\n\
        \........#.\n\
        \#.........\n\
        \......#..."

minSafe, maxSafe :: (Ord a) => [a] -> Maybe a
minSafe [] = Nothing
minSafe ls = Just $ minimum ls
maxSafe [] = Nothing
maxSafe ls = Just $ maximum ls

move :: Coord -> Direction -> Field -> Either Coord Coord
move (x, y) R f = maybe (Left ((fst . fieldMax) f, y)) Right $ first (\x -> x - 1) <$> (minSafe . filter (\(x', y') -> y' == y && x' > x)) f
move (x, y) L f = maybe (Left (0, y)) Right $ first (\x -> x + 1) <$> (maxSafe . filter (\(x', y') -> y' == y && x' < x)) f
move (x, y) U f = maybe (Left (x, 0)) Right $ second (\y -> y + 1) <$> (maxSafe . filter (\(x', y') -> y' < y && x' == x)) f
move (x, y) D f = maybe (Left (x, (snd . fieldMax) f)) Right $ second (\y -> y - 1) <$> (minSafe . filter (\(x', y') -> y' > y && x' == x)) f

turn90 :: Direction -> Direction
turn90 R = D
turn90 L = U
turn90 U = R
turn90 D = L

fieldMax :: Field -> Coord
fieldMax f = bimap maximum maximum (fmap fst f, fmap snd f)

walk :: Coord -> Direction -> Field -> [Coord]
walk c d f = case mayNextPosition of
    Right c' -> stepsBetweenPoints c c' ++ walk c' nextDirection f
    Left outOfBorder -> stepsBetweenPoints c outOfBorder
  where
    mayNextPosition = move c d f
    nextDirection = turn90 d
    stepsBetweenPoints (x, y) (a, b) = [(c, d) | c <- [min x a .. max x a], d <- [min y b .. max y b]]

solution1 :: (Field, Coord) -> Int
solution1 (f, c) = length . nub $ walk c U f

-- Too high 5969, 5479
december06Solution1 :: IO Int
december06Solution1 = solution1 <$> input

move4 :: Coord -> Direction -> Field -> [Either Coord Coord]
move4 c d f = fmap fst . take 3 $ iterate (\(ec, d') -> (ec >>= \x -> move x d' f, turn90 d')) (Right c ,d)

walk' :: Coord -> Direction -> Field -> [Coord] -> [Coord] --Int
walk' c d f path = case mayNextPosition of
    Right c' -> loopIntersections c' ++ walk' c' nextDirection f (path ++ stepsBetweenPoints c c')
    Left outOfBorder -> loopIntersections outOfBorder
  where
    mayNextPosition = move c d f
    nextDirection = turn90 d
    loopIntersections p =
        ---length $
             filter
                ( \(x, y) ->
                    (x `elem` fmap fst f || y `elem` fmap snd f)
                    --(x `elem` fmap fst path || y `elem` fmap snd path)
                        && isRight (move (x, y) nextDirection f)
                        && any (`elem` path) (rights (move4 (x, y) nextDirection f))
                )
            $ stepsBetweenPoints c p
    stepsBetweenPoints (x, y) (a, b) = [(c, d) | c <- [min x a .. max x a], d <- [min y b .. max y b]]

-- solution2 :: (Field, Coord) -> Int
solution2 (f, c) = walk' c U f []

-- too low 1031, 1688
-- december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
