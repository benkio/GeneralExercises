module TwentyTwentyFour.December06 where

import Text.Printf (printf)

import Data.Bifunctor (bimap, first, second)
import Data.Either (either, isRight, partitionEithers, rights)
import Data.List (isSubsequenceOf, nub)
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
move (x, y) R f =
    maybe
        (Left ((fst . fieldMax) f, y))
        (Right . first (\x -> x - 1))
        ((minSafe . filter (\(x', y') -> y' == y && x' > x)) f)
move (x, y) L f =
    maybe
        (Left (0, y))
        (Right . first (+ 1))
        ((maxSafe . filter (\(x', y') -> y' == y && x' < x)) f)
move (x, y) U f =
    maybe
        (Left (x, 0))
        (Right . second (+ 1))
        ((maxSafe . filter (\(x', y') -> y' < y && x' == x)) f)
move (x, y) D f =
    maybe
        (Left (x, (snd . fieldMax) f))
        (Right . second (\y -> y - 1))
        ((minSafe . filter (\(x', y') -> y' > y && x' == x)) f)

turn90 :: Direction -> Direction
turn90 R = D
turn90 L = U
turn90 U = R
turn90 D = L

fieldMax :: Field -> Coord
fieldMax f = bimap maximum maximum (fmap fst f, fmap snd f)

walk :: Coord -> Direction -> Field -> [Coord] -> [Coord]
walk c d f path
    | loopDetection f path = path
    | otherwise =
        either
            (\outOfBorder -> path ++ stepsBetweenPoints c outOfBorder)
            (\c' -> walk c' nextDirection f (path ++ stepsBetweenPoints c c'))
            eitherNextPosition
  where
    eitherNextPosition = move c d f
    nextDirection = turn90 d
    stepsBetweenPoints (x, y) (a, b) = [(c, d) | c <- [min x a .. max x a], d <- [min y b .. max y b]]

solution1 :: (Field, Coord) -> Int
solution1 (f, c) = length . nub $ walk c U f []

-- Too high 5969, 5479
december06Solution1 :: IO Int
december06Solution1 = solution1 <$> input

loopDetection :: Field -> Field -> Bool
loopDetection f loopCheck = length loopCheck > maxSteps
  where
    maxSteps = uncurry (*) $ fieldMax f

addObstacles :: Field -> Coord -> Field -> [Field]
addObstacles f c referencePath = (: f) <$> obstaclesPosition
  where
    (xmf, ymf) = fieldMax f
    obstaclesPosition = [(x, y) | x <- [0 .. xmf], y <- [0 .. ymf], (x, y) `notElem` f, (x, y) /= c, (x, y) `elem` referencePath]

solution2 :: (Field, Coord) -> Int
solution2 (f, c) = foldl (\acc f' -> if loopDetection f' (walk c U f' []) then acc + 1 else acc) 0 fieldsWithObstacles
  where
    referencePath = walk c U f []
    fieldsWithObstacles = addObstacles f c referencePath

-- too low 1031, 1688
-- 1946
december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
