module TwentyTwentyTwo.December17 where

import Data.Bifoldable (bifoldMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (elem, find, intersperse, minimum, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Printf (printf)
import Prelude hiding (lookup)

data Rock = HLine | Plus | LShape | VLine | Square deriving (Show, Eq)

data HotGas = R | L deriving (Show, Eq)

data State = State
    { rocks :: [Rock]
    , chamber :: IntSet
    , hotGas :: [HotGas]
    , fallenBlocks :: Int
    }

initialState hs = State{rocks = rockStream, chamber = IntSet.empty, hotGas = hs, fallenBlocks = 0}

instance Show State where
    show s = printf "State: %d" (fallenBlocks s)

instance Eq State where
    (==) s s' =
        IntSet.map (\x -> x - IntSet.findMax (chamber s)) (chamber s) == IntSet.map (\x -> x - IntSet.findMax (chamber s')) (chamber s')
            && (head . rocks) s == (head . rocks) s'
            && (take 5 . hotGas) s == (take 5 . hotGas) s'

input :: IO [HotGas]
input = cycle . fmap parseHotGas . init <$> readFile "input/2022/17December.txt"

rockStream :: [Rock]
rockStream = cycle [HLine, Plus, LShape, VLine, Square]

indexX :: Int -> Int
indexX = (`mod` 10)

indexY :: Int -> Int
indexY = (* 10) . (`div` 10)

rockWidth :: Rock -> Int
rockWidth HLine = 4
rockWidth Plus = 3
rockWidth LShape = 3
rockWidth VLine = 1
rockWidth Square = 2

rockHeight :: Rock -> Int
rockHeight HLine = 1
rockHeight Plus = 3
rockHeight LShape = 3
rockHeight VLine = 4
rockHeight Square = 2

parseHotGas :: Char -> HotGas
parseHotGas '<' = L
parseHotGas '>' = R
parseHotGas x = error $ printf "char %c is not recognized" x

testInput :: [HotGas]
testInput = cycle $ parseHotGas <$> ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

fallOffset :: Int -> IntSet -> Int
fallOffset i is
    | i == 0 && IntSet.null is = 20
    | otherwise = i + 30

initialFallPosition :: Rock -> Int -> IntSet -> Int
initialFallPosition HLine i is = fallOffset i is + 12
initialFallPosition Plus i is = fallOffset i is + 32
initialFallPosition LShape i is = fallOffset i is + 32
initialFallPosition VLine i is = fallOffset i is + 42
initialFallPosition Square i is = fallOffset i is + 22

rockSpace :: Rock -> Int -> [Int]
rockSpace HLine i = [i + x | x <- [0 .. 3]]
rockSpace Plus i = [i + 1, i - 10, i - 9, i - 8, i - 19]
rockSpace VLine i = [i - (10 * x) | x <- [0 .. 3]]
rockSpace Square i = [i, i + 1, i - 10, i - 9]
rockSpace LShape i = [i + 2, i - 8, i - 18, i - 19, i - 20]

collision :: Rock -> Int -> IntSet -> Bool
collision r p is = sideCollision r || bottomCollision r || rockCollision r is
  where
    sideCollision x = (rockWidth x - 1) + indexX p > 6 || (indexX p `elem` [7 .. 9])
    bottomCollision x = indexY p - ((rockHeight x - 1) * 10) < 0
    rockCollision x s = any (`IntSet.member` s) $ rockSpace x p

hotGasShift :: HotGas -> Int
hotGasShift L = -1
hotGasShift R = 1

hotGasMovement :: Rock -> Int -> HotGas -> IntSet -> Int
hotGasMovement r p h is = if collision r p' is then p else p'
  where
    p' = p + hotGasShift h

downwardMovement :: Rock -> Int -> IntSet -> Maybe Int
downwardMovement r p is = if collision r (p - 10) is then Nothing else Just (p - 10)

singleFall :: Rock -> Int -> IntSet -> HotGas -> Either Int Int
singleFall r p is h = maybe (Left hotGasMP) Right downwardMP
  where
    hotGasMP = hotGasMovement r p h is
    downwardMP = downwardMovement r hotGasMP is

rockSettle :: Rock -> Int -> IntSet -> IntSet
rockSettle r i is = foldl (flip IntSet.insert) is $ rockSpace r i

rockFall :: Rock -> Int -> IntSet -> [HotGas] -> (IntSet, [HotGas])
rockFall r p is (h : hs) =
    case fall of
        Left p' -> (rockSettle r p' is, hs)
        Right p' -> rockFall r p' is hs
  where
    fall = singleFall r p is h

filterMinY :: IntSet -> IntSet
filterMinY is = (snd . IntSet.split (minMaxY - 30)) is
  where
    maxByColumn x =
        let column = IntSet.filter ((== x) . indexX) is
         in if IntSet.null column then 0 else IntSet.findMax (IntSet.map indexY column)
    maxColumns = fmap maxByColumn [0 .. 6]
    minMaxY = minimum maxColumns

freeFall :: State -> State
freeFall State{rocks = (r : rs), chamber = is, hotGas = hs, fallenBlocks = fb} =
    State{rocks = rs, chamber = filterMinY is', hotGas = hs', fallenBlocks = fb + 1}
  where
    m = if IntSet.null is then 0 else (indexY . IntSet.findMax) is
    (is', hs') = rockFall r (initialFallPosition r m is) is hs

calculateChamberHeight :: State -> Int
calculateChamberHeight =
    (+ 1)
        . (`div` 10)
        . indexY
        . IntSet.findMax
        . chamber

solution1 :: Int -> [HotGas] -> Int
solution1 limit hs =
    ( calculateChamberHeight
        . until ((== limit) . fallenBlocks) freeFall
    )
        (initialState hs)

december17Solution1 :: IO Int
december17Solution1 = solution1 2022 <$> input

findCycle :: [HotGas] -> (State, State)
findCycle hs = findCycle' (initialState hs) []

findCycle' :: State -> [State] -> (State, State)
findCycle' s st
    | s `elem` st = (s, fromJust (find (== s) st))
    | otherwise = findCycle' (freeFall s) (s : st)

solution2 :: [HotGas] -> Int
solution2 hots = hs + (fbic * ch) + (hs'' - hs')
  where
    (s, s') = findCycle hots
    hs = calculateChamberHeight s
    hs' = calculateChamberHeight s'
    ch = hs - hs'
    cfb = fallenBlocks s - fallenBlocks s'
    (fbic, lfb) = (1000000000000 - fallenBlocks s) `divMod` cfb
    (s'', _) = until (\(_, b) -> b == 0) (\(x, b) -> (freeFall x, b - 1)) (s', lfb)
    hs'' = calculateChamberHeight s''

-- 1581512605025 too High
-- 1581512604084 too High
december17Solution2 :: IO Int
december17Solution2 = solution2 <$> input

showChamber :: IntSet -> String
showChamber is = (printStuff . IntSet.toAscList) is
  where
    m = (indexY . IntSet.findMax) is
    validIdexes = (chunksOf 7 . sort) [x + (10 * y) | x <- [0 .. 6], y <- [0 .. (m `div` 10)]]
    printStuff l =
        ( concat
            . reverse
            . intersperse ['\n']
            . fmap (fmap (\i -> if i `elem` l then '#' else '.'))
        )
            validIdexes
