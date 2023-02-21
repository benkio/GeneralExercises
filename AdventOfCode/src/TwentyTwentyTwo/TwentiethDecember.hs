module TwentyTwentyTwo.TwentiethDecember where

import Data.Maybe (fromJust)
import Data.Vector (Vector, cons, elemIndex, fromList, singleton, snoc)
import qualified Data.Vector as V (concat, filter, head, length, splitAt, tail, (!))
import Debug.Trace

parseInput = fmap (\x -> (read x :: Int)) . lines

input :: IO [Int]
input = parseInput <$> readFile "input/2022/20December.txt"

testInput :: [Int]
testInput =
    parseInput
        "1\n\
        \2\n\
        \-3\n\
        \3\n\
        \-2\n\
        \0\n\
        \4"

-- from a value return the starting and ending indexes of the move
calculateIndexes :: Int -> Int -> Vector Int -> (Int, Int)
calculateIndexes prevOffset x v
    | prevOffset == (abs x) = (i, offset)
    | offset <= 0 = (\(_, e) -> (i, e)) $ calculateIndexes (i + prevOffset) x v'
    | offset >= vl = (\(_, e) -> (i, e)) $ calculateIndexes (vl - 1 - i + prevOffset) x v''
    | otherwise = (i, offset)
  where
    vl = V.length v
    i = (fromJust . elemIndex x) v
    offset = if x < 0 then i + x + prevOffset else i + x - prevOffset
    v' = snoc (V.filter (/= x) v) x
    v'' = cons x $ V.filter (/= x) v

moveNumber :: Int -> Int -> Vector Int -> Vector Int
moveNumber si ei v
    | si < ei =
        ( ( \(pre, post) ->
                let (prepre, postpre) = V.splitAt si pre
                 in V.concat [prepre, V.tail postpre, singleton (V.head postpre), post]
          )
            . V.splitAt (ei + 1)
        )
            v
    | si > ei =
        ( ( \(pre, post) ->
                let (prepost, postpost) = V.splitAt (si - ei) post
                 in V.concat [pre, singleton (V.head postpost), prepost, V.tail postpost]
          )
            . V.splitAt ei
        )
            v
    | otherwise = v

moveCycle :: [Int] -> Vector Int -> Vector Int
moveCycle [] v = v
moveCycle (x : xs) v = moveCycle xs v'
  where
    (si, ei) = calculateIndexes 0 x v
    v' = moveNumber si ei v

findCoordinates :: Vector Int -> [Int]
findCoordinates v =
    [ coordinateFromZero 1000 v
    , coordinateFromZero 2000 v
    , coordinateFromZero 3000 v
    ]

coordinateFromZero :: Int -> Vector Int -> Int
coordinateFromZero x v = (V.!) v i
  where
    zi = (fromJust . elemIndex 0) v
    i = mod (x - zi + 1) (V.length v)

solution :: [Int] -> Int
solution i = (sum . findCoordinates . moveCycle i) $ fromList i

-- -2257 wrong
twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution <$> input

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = undefined

testExample :: Bool
testExample =
    solution testInput == 3
        && ((findCoordinates . moveCycle testInput) (fromList testInput)) == [4, -3, 2]
        && ((moveCycle testInput) (fromList testInput)) == (fromList [1, 2, -3, 4, 0, 3, -2])

test2 :: Bool
test2 = ((moveCycle testCase) (fromList testCase)) == (fromList [0, -5, 9])
  where
    testCase = [9, 0, -5]
