-------------------------------------------------------------------------------
--                           Advent Of Code - day 23                          --
-------------------------------------------------------------------------------
module TwentyTwenty.December23 where

import Data.Char (digitToInt)
import Data.Vector as V (
    Vector,
    length,
    modify,
    replicate,
    (!),
    (//),
 )
import Data.Vector.Mutable (write)

input :: IO [Int]
input = fmap digitToInt . init <$> readFile "input/2020/23December.txt"

buildVector :: [Int] -> Vector Int
buildVector l =
    V.replicate (Prelude.length l + 1) 0 // (lzipped ++ [(last l, head l)])
  where
    lzipped = l `zip` Prelude.tail l

inputTest :: Vector Int
inputTest = buildVector [3, 8, 9, 1, 2, 5, 4, 6, 7]

rebuildSequenceStartingFrom :: Int -> Vector Int -> [Int]
rebuildSequenceStartingFrom cursor xs =
    value : rebuildSequenceStartingFrom value xs
  where
    value = xs ! cursor

selectDestinationCupIndex :: Int -> [Int] -> Vector Int -> Int
selectDestinationCupIndex currentCup extractedCups xs
    | candidate `notElem` extractedCups && candidate > 0 = candidate
    | candidate == 0 = selectDestinationCupIndex (V.length xs) extractedCups xs
    | otherwise = selectDestinationCupIndex candidate extractedCups xs
  where
    candidate = currentCup - 1

crubSingleMove :: Int -> Int -> Vector Int -> (Int, Vector Int)
crubSingleMove iteration currentCup cups =
    let a = cups ! currentCup
        b = cups ! a
        c = cups ! b
        dest = selectDestinationCupIndex currentCup [a, b, c] cups
        afterDestinationCupPointer = cups ! dest
        afterLastElemToMovePointer = cups ! c
        newCups =
            modify
                ( \v -> do
                    write v currentCup afterLastElemToMovePointer --  cups[cup] = cups[c];
                    write v dest a -- cups[dest] = a;
                    write v c afterDestinationCupPointer -- cups[c] = temp;
                )
                cups
        newCurrentCup = newCups ! currentCup
     in crubSingleMove (iteration - 1) newCurrentCup newCups

-- crubMoves :: Int -> Vector Int -> [(Int, Vector Int)]
-- crubMoves selectedCup cups =
--   iterate (uncurry crubSingleMove) (selectedCup, cups)

-- crubMovesLimit :: Int -> Int -> Vector Int -> (Int, Vector Int)
-- crubMovesLimit movesNum startingCup cups =
--   crubMoves startingCup cups !! movesNum

cupsToString :: Vector Int -> String
cupsToString = concatMap show . takeWhile (1 /=) . rebuildSequenceStartingFrom 1

twentyThirdDecemberSolution1 :: IO String
twentyThirdDecemberSolution1 =
    cupsToString . snd . (\l -> crubSingleMove 100 (head l) (buildVector l))
        <$> input

expandInputToOneMilion :: [Int] -> Vector Int
expandInputToOneMilion xs = buildVector $ xs ++ [maximum xs + 1 .. 1000000]

solution2 =
    ( \v ->
        let a = v ! 1
            b = v ! b
         in a * b
    )
        . snd
        . (\l -> crubSingleMove 10000000 (head l) (expandInputToOneMilion l))

-- This failed!!! It just run and never return, I did it in scala, 30 sec -.-"
twentyThirdDecemberSolution2 :: IO Int
twentyThirdDecemberSolution2 = solution2 <$> input
