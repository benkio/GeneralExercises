module TwentyTwentyTwo.SeventeenthDecember where

import Data.List (find, sortOn)
import Data.Map (Map, adjust, filterWithKey, fromList, lookup, toList, union, findMin, findMax, mapKeys, empty)
import Text.Printf (printf)
import Prelude hiding (lookup)
import Debug.Trace

data Rock = HLine | Plus | LShape | VLine | Square deriving (Show)
data HotGas = R | L deriving Show
data Space = Empty | Occupied
type Position = (Int, Int)

instance Show Space where
    show Empty = "."
    show Occupied = "#"

input :: IO [HotGas]
input = cycle . fmap parseHotGas . init <$> readFile "input/2022/17December.txt"

rockStream :: [Rock]
rockStream = cycle [HLine, Plus, LShape, VLine, Square]

isOccupied :: Space -> Bool
isOccupied Occupied = True
isOccupied _ = False

lowestOccupiedSpaceY :: Map Position Space -> Int
lowestOccupiedSpaceY = maybe 0 (\((_, y),_) -> y) . find (\((_, _),s) -> isOccupied s) . sortOn (snd . fst) . toList

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

initialPosition :: Position
initialPosition = (2, 0)

rockInitialBoard :: Rock -> Map Position Space
rockInitialBoard r = rockBoard r `union` emptyLines (rockHeight r)
  where
    emptyLines sy = fromList [((x, y), Empty) | x <- [0 .. 6], y <- [sy .. sy + 2]]
    rockBoard r =
      -- foldl setSingleRock (
      emptyBoard (rockHeight r)-- ) $ rockSpace r initialPosition
    --setSingleRock m' rp = adjust (const Occupied) rp m'

emptyBoard :: Int -> Map Position Space
emptyBoard h = fromList [((x, y), Empty) | x <- [0 .. 6], y <- [0 .. h - 1]]

rockSpace :: Rock -> Position -> [Position]
rockSpace HLine (x, y) = [(x + i, y) | i <- [0 .. 3]]
rockSpace Plus (x, y) = [(x + 1, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 1, y + 2)]
rockSpace VLine (x, y) = [(x, y + i) | i <- [0 .. 3]]
rockSpace Square (x, y) = [(x + ix, y + iy) | ix <- [0, 1], iy <- [0, 1]]
rockSpace LShape (x, y) = [(x + 2, y), (x + 2, y + 1), (x + 2, y + 2), (x + 1, y + 2), (x, y + 2)]

rockCollision :: Rock -> Position -> Map Position Space -> Bool
rockCollision r p m = any (\rp -> maybe True isOccupied (lookup rp m)) $ rockSpace r p

settleRock :: Rock -> Position -> Map Position Space -> Map Position Space
settleRock r p m = filterWithKey (\k _ -> snd k >= minY) resultM
  where
    setSingleRock m' rp = adjust (const Occupied) rp m'
    resultM = foldl setSingleRock m $ rockSpace r p
    minY = lowestOccupiedSpaceY resultM

downwardMovement :: Rock -> Position -> Map Position Space -> Maybe Position
downwardMovement r (x, y) m
    | rockCollision r (x, y + 1) m = Nothing
    | otherwise = Just (x, y + 1)

hotGasHorizontalMovement :: Rock -> Position -> HotGas -> Map Position Space -> Position
hotGasHorizontalMovement r p L m = hotGasHorizontalMovementLeft r p m
hotGasHorizontalMovement r p R m = hotGasHorizontalMovementRight r p m

hotGasHorizontalMovementLeft :: Rock -> Position -> Map Position Space -> Position
hotGasHorizontalMovementLeft r p@(x, y) m
    | rockCollision r (x - 1, y) m = p
    | otherwise = (x - 1, y)
hotGasHorizontalMovementRight :: Rock -> Position -> Map Position Space -> Position
hotGasHorizontalMovementRight r p@(x, y) m
    | rockCollision r (x + 1, y) m = p
    | otherwise = (x + 1, y)

fallStep :: Rock -> Position -> HotGas -> Map Position Space -> Either Position Position
fallStep r p h m = maybe (Left hotGasPosition) Right downwardPosition
  where
    hotGasPosition = hotGasHorizontalMovement r p h m
    downwardPosition = downwardMovement r hotGasPosition m

singleFall :: Rock -> Position -> [HotGas] -> Map Position Space -> (Map Position Space, [HotGas])
singleFall r p (h : hs) m = case fallStepPosition of
    Left p' -> (settleRock r p' m, hs)
    Right p' -> singleFall r p' hs m
  where
    fallStepPosition = fallStep r p h m

freeFall :: [Rock] -> Int -> Map Position Space -> [HotGas] -> Map Position Space
freeFall _ 0 m _ =
  let minY = lowestOccupiedSpaceY m
  in  mapKeys (\(x,y) -> (x,y-minY)) m
freeFall (r:rs) b m hs = freeFall rs (traceShowId (b-1)) m'' hs'
  where
    ((_, maxY), _) = findMax $ rockInitialBoard r
    ((_, minY), _) = if null m then ((0,0), Empty) else findMin m
    m' = rockInitialBoard r `union` mapKeys (\(x,y) -> (x, y-minY+maxY+1)) m
    (m'', hs') = singleFall r initialPosition hs m'

solution1 :: Int -> [HotGas] -> Int
solution1 limit hs = calcHeight $ freeFall rockStream limit empty hs
  where calcHeight = (+1) . snd . fst . traceShowId . findMax

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = solution1 2022 <$> input

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = solution1 1000000000000 <$> input

showGrid :: Map Position Space -> String
showGrid = showGrid' . sortOn (snd . fst) . toList

showGrid' grid =
    fst $
        foldl
            ( \(acc, y) ((_, y'), s) ->
                if y /= y'
                    then (acc ++ "\n" ++ show s, y')
                    else (acc ++ show s, y)
            )
            ("", 0)
            grid
