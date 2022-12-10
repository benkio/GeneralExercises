-------------------------------------------------------------------------------
--                           Advent Of Code - day 25                          --
-------------------------------------------------------------------------------
module TwentyTwenty.TwentyFifthDecember where

import Data.List (find)
import Data.Maybe (fromJust)

input :: IO (Int, Int)
input =
    (\l -> (read (head l) :: Int, read (l !! 1) :: Int)) . lines
        <$> readFile "input/2020/25December.txt"

subjectNumber :: Int
subjectNumber = 7

moduloNumber :: Int
moduloNumber = 20201227

findLoopSize :: Int -> Int
findLoopSize targetRemainder =
    ( fst
        . fromJust
        . find ((targetRemainder ==) . snd)
        . zip [0 ..]
        . scanl (\acc _ -> (acc * 7) `mod` moduloNumber) 1
    )
        [1 ..]

generateEncriptionKey :: Int -> Int -> Int -> Int
generateEncriptionKey value publicKey loopSize =
    if loopSize == 0
        then value
        else generateEncriptionKey (value * publicKey `mod` moduloNumber) publicKey (loopSize - 1)

solution1 :: Int -> Int -> Int
solution1 publicKey1 publicKey2 =
    let loopSize1 = findLoopSize publicKey1
        loopSize2 = findLoopSize publicKey2
        encriptionKey1 = generateEncriptionKey 1 publicKey1 loopSize2
        encriptionKey2 = generateEncriptionKey 1 publicKey2 loopSize1
     in if encriptionKey1 == encriptionKey2
            then encriptionKey1
            else error "could not find an encription key"

solution1Test :: Bool
solution1Test = uncurry solution1 inputTest == 14897079

inputTest :: (Int, Int)
inputTest = (5764801, 17807724)

twentyFifthDecemberSolution1 :: IO Int
twentyFifthDecemberSolution1 = uncurry solution1 <$> input
