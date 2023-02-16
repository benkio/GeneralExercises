module TwentyTwentyTwo.TwentiethDecember where

import Data.Maybe (fromJust)
import Data.Vector (Vector, elemIndex, fromList, singleton)
import qualified Data.Vector as V (concat, head, length, splitAt, tail, (!))
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
calculateIndexes :: Int -> Vector Int -> (Int, Int)
calculateIndexes x v
    | (i + x) < 0 = (i, headToTail (ei - 1))
    | (i + x) >= vl = (i, headToTail (ei + 1))
    | otherwise = (i, headToTail ei)
  where
    vl = V.length v
    i = (fromJust . elemIndex x) v
    ei = mod (i + x) vl
    headToTail a = if a == 0 then vl else a

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
    (si, ei) = calculateIndexes x v
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
test2 =
    solution testCase == 3
    && ((findCoordinates . moveCycle testCase) (fromList testCase)) == [4, -3, 2]
    && ((moveCycle testCase) (fromList testCase)) == (fromList [0, -5, 9])
  where
    testCase = [9, 0, -5]
