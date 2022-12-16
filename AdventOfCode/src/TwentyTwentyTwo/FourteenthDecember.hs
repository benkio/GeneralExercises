{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyTwo.FourteenthDecember where

import Data.Foldable (foldlM)
import Data.List (maximumBy, minimumBy, nub, sortOn)
import Data.Map (Map, empty, filter, filterWithKey, findMax, findMin, insert, keys, lookup, size)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text, pack, splitOn, unpack)
import Text.Printf
import Prelude hiding (filter, lookup)

type Position = (Int, Int)
type Rock = [Position]
data Tile = Rock | Sand deriving (Eq)
type Waterfall = Map Position Tile
data SandStatus = Rest Waterfall | NoMoreSand deriving (Show)

instance Show Tile where
    show Rock = "#"
    show Sand = "○"

input :: IO Waterfall
input = toWaterfall . fmap expandRock . parseRocks <$> readFile "input/2022/14December.txt"

moveSand :: Waterfall -> Position -> Either SandStatus (Waterfall, Position)
moveSand m p
    | (snd p + 1) > maxY = Left NoMoreSand
    | isNothing below = Right (m, positionBelow p)
    | isNothing belowLeft = Right (m, positionBelowLeft p)
    | isNothing belowRight = Right (m, positionBelowRight p)
    | otherwise = Left $ Rest $ insert p Sand m
  where
    (_, maxY) = (maximumBy (\(_, y) (_, y') -> y `compare` y') . keys) m
    below = lookup (positionBelow p) m
    belowLeft = lookup (positionBelowLeft p) m
    belowRight = lookup (positionBelowRight p) m

moveAllSand :: Waterfall -> Waterfall
moveAllSand m = go m (500, 0)
  where
    go m p = case moveSand m p of
        Left NoMoreSand -> m
        Left (Rest m') -> go m' (500, 0)
        Right (m', p') -> go m' p'

countSand :: Waterfall -> Int
countSand = size . filter isSand

solution :: (Waterfall -> Waterfall) -> Waterfall -> Int
solution mas = countSand . mas

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 = solution moveAllSand <$> input

moveSand' :: Waterfall -> Position -> Either SandStatus (Waterfall, Position)
moveSand' m p
    | isNothing below && snd p < maxY + 1 = Right (m, nextBelow)
    | isNothing belowLeft && snd p < maxY + 1 = Right (m, nextBelowLeft)
    | isNothing belowRight && snd p < maxY + 1 = Right (m, nextBelowRight)
    | snd p == 0 && fst p == 500 && below == Just Sand && belowLeft == Just Sand && belowRight == Just Sand = Left NoMoreSand
    | otherwise = Left $ Rest $ insert p Sand m
  where
    (x, maxY) = (maximumBy (\(_, y) (_, y') -> y `compare` y') . keys . filter isRock) m
    below = lookup (positionBelow p) m
    nextBelow = until (\(x, y) -> isJust (positionBelow (x, y) `lookup` m) || y == maxY + 1) positionBelow p
    belowLeft = lookup (positionBelowLeft p) m
    nextBelowLeft = until (\(x, y) -> isJust (positionBelowLeft (x, y) `lookup` m) || y == maxY + 1) positionBelowLeft p
    belowRight = lookup (positionBelowRight p) m
    nextBelowRight = until (\(x, y) -> isJust (positionBelowRight (x, y) `lookup` m) || y == maxY + 1) positionBelowRight p

moveAllSand' :: Waterfall -> Waterfall
moveAllSand' m = go m (500, 0)
  where
    go m p = case moveSand' m p of
        Left NoMoreSand -> m
        Left (Rest m') -> go m' (500, 0)
        Right (m', p') -> go m' p'

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 = (+ 1) . solution moveAllSand' <$> input

---------------------------------------------

isSand :: Tile -> Bool
isSand Sand = True
isSand _ = False

isRock :: Tile -> Bool
isRock Rock = True
isRock _ = False

positionBelow :: Position -> Position
positionBelow (x, y) = (x, y + 1)

positionBelowLeft :: Position -> Position
positionBelowLeft (x, y) = (x - 1, y + 1)

positionBelowRight :: Position -> Position
positionBelowRight (x, y) = (x + 1, y + 1)

-- parsing --------------------------------------

parseRocks :: String -> [Rock]
parseRocks = fmap (fmap toPosition . parseRockLine) . lines
  where
    parseRockLine :: String -> [Text]
    parseRockLine = splitOn " -> " . pack
    toPosition :: Text -> Position
    toPosition = (\xs -> (read (unpack (head xs)) :: Int, read (unpack (last xs)) :: Int)) . splitOn ","

testInput :: Waterfall
testInput =
    (toWaterfall . fmap expandRock . parseRocks)
        "498,4 -> 498,6 -> 496,6\n\
        \503,4 -> 502,4 -> 502,9 -> 494,9"

toWaterfall :: [Rock] -> Waterfall
toWaterfall = foldl (foldl (\m' p -> insert p Rock m')) empty

expandRock :: Rock -> Rock
expandRock rs = nub $ (rs `zip` tail rs) >>= fillLine
  where
    fillLine :: (Position, Position) -> Rock
    fillLine ((x, y), (a, b))
        | x == a = if y <= b then [(x, ry) | ry <- [y .. b]] else [(x, ry) | ry <- [b .. y]]
        | y == b = if x <= a then [(rx, y) | rx <- [x .. a]] else [(rx, y) | rx <- [a .. x]]
        | otherwise = error $ printf "Not a line: (%s, %s)" (show (x, y)) (show (a, b))

drawWaterfall :: Waterfall -> IO ()
drawWaterfall m =
    let (minX, _) = (minimumBy (\(x, _) (x', _) -> x `compare` x') . keys) m
        (maxX, _) = (maximumBy (\(x, _) (x', _) -> x `compare` x') . keys) m
        (_, maxY) = (maximumBy (\(_, y) (_, y') -> y `compare` y') . keys) m
     in foldlM
            ( \_ y ->
                foldlM
                    ( \_ x ->
                        if (x, y) == (500, 0)
                            then putStr "+"
                            else putStr (maybe "." show (lookup (x, y) m))
                    )
                    ()
                    [minX .. maxX]
                    >> putStr "\n"
            )
            ()
            [0 .. maxY]
