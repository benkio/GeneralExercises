module TwentyTwentyThree.TwentySecondDecember where

import Text.Printf (printf)

import Data.Bifunctor (bimap)
import Data.Either (isLeft, isRight)
import Data.List (find, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import qualified Data.Set as S (Set, empty, insert, size)
import Debug.Trace

type Coord = (Int, Int, Int)
data Block = B {bid :: Int, cs :: [Coord]} deriving (Show)
type Tetris = [Block]

instance Eq Block where
    (==) (B{bid = b1}) (B{bid = b2}) = b1 == b2

instance Ord Block where
    (<=) (B{bid = b1}) (B{bid = b2}) = b1 <= b2

getBricksByLevel :: Tetris -> Int -> [Coord]
getBricksByLevel ts z = concatMap (filter (\(_, _, bz) -> bz == z) . cs) ts
getBlockByLevel :: Tetris -> Int -> [Block]
getBlockByLevel ts z = filter ((== z) . minimum . fmap (\(_, _, bz) -> bz) . cs) ts
getMaximumBlockLevel :: Tetris -> Int
getMaximumBlockLevel = maximum . fmap (\(_, _, z) -> z) . concatMap cs

getMinimumBlockLevel :: Block -> Int
getMinimumBlockLevel = minimum . fmap (\(_, _, z) -> z) . cs

moveBlocksBelow :: Tetris -> [Either Block Block]
moveBlocksBelow ts = fmap compute ts
  where
    belowLevel b = getMinimumBlockLevel b - 1
    newPosition b = fmap (\(x, y, z) -> (x, y, z - 1)) (cs b)
    isBottom :: Block -> Bool
    isBottom = any (\(_, _, z) -> z == 0) . newPosition
    compute b =
        if isBottom b || any (`elem` newPosition b) (getBricksByLevel ts (belowLevel b))
            then Left b
            else Right $ B{bid = bid b, cs = newPosition b}

moveTetris :: Tetris -> (Tetris, S.Set Block)
moveTetris tetris = go tetris S.empty
  where
    go :: Tetris -> S.Set Block -> (Tetris, S.Set Block)
    go ts rs =
        let
            nts = moveBlocksBelow ts
            lefts = filter isLeft nts
            leftsLength = length lefts
            rights = filter isRight nts
            rightsLength = length rights
         in
            if rightsLength > 0
                then
                    trace (printf "debug: moved, leftsLength %d rightsLength %d" leftsLength rightsLength) $
                        go
                            (fmap (either id id) nts)
                            (foldl (\s e -> S.insert (either id id e) s) rs rights)
                else trace (printf "debug: all fallen") (ts, rs)

destroyableBlocks :: Tetris -> Int
destroyableBlocks ts = length . filter (all isLeft) $ fmap (\i -> moveBlocksBelow (deleteAt i ts)) [0 .. length ts - 1]

deleteAt idx xs = lft ++ rgt
  where
    (lft, _ : rgt) = splitAt idx xs

input :: IO Tetris
input = sortOn getMinimumBlockLevel . parseInput <$> readFile "input/2023/22December.txt"

parseInput :: String -> Tetris
parseInput = zipWith parseBlock [1 ..] . lines
  where
    parseBlock :: Int -> String -> Block
    parseBlock blockId s =
        let (startBlock, endBlock) = (bimap (fmap (\x -> read x :: Int) . splitOn ",") (fmap (\x -> read x :: Int) . splitOn "," . tail) . break (== '~')) s
            [xs, ys, zs] = zipWith (\s e -> [s .. e]) startBlock endBlock
         in B{bid = blockId, cs = [(x, y, z) | x <- xs, y <- ys, z <- zs]}

testInput :: Tetris
testInput =
    parseInput
        "1,0,1~1,2,1\n\
        \0,0,2~2,0,2\n\
        \0,2,3~2,2,3\n\
        \0,0,4~0,2,4\n\
        \2,0,5~2,2,5\n\
        \0,1,6~2,1,6\n\
        \1,1,8~1,1,9"

solution1 :: Tetris -> Int
solution1 = destroyableBlocks . fst . moveTetris

twentysecondDecemberSolution1 :: IO Int
twentysecondDecemberSolution1 = solution1 <$> input

fallBricks :: Tetris -> Int
fallBricks ts =
    sum $ fmap (S.size . snd . moveTetris . (`deleteAt` ts)) [0 .. length ts - 1]

solution2 = fallBricks . fst . moveTetris

twentysecondDecemberSolution2 :: IO Int
twentysecondDecemberSolution2 = solution2 <$> input
