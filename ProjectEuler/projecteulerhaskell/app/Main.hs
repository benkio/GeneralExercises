module Main where

import ProjectEuler
import ProjectEuler2
import ProjectEuler3
import System.TimeIt
import Text.Printf

main :: IO ()
main = do
    putStrLn "--------------------Project Euler 1-10---------------"
    -- printf "Es1: %d Es2: %d Es3: %d Es4: %d Es5: %d \n" es1 es2 es3 es4 es5
    printExercise es1 1
    printExercise es2 2
    printExercise es3 3
    printExercise es4 4
    printExercise es5 5
    putStrLn ""
    printExercise es6 6
    printExercise es7 7
    printExercise es8 8
    printExercise es9 9
    printExercise es10 10
    putStrLn ""
    putStrLn "--------------------Project Euler 11-20--------------"
    printExercise es11 11
    printExercise es12 12
    printExerciseStr es13 13
    printExercise es14 14
    printExercise es15 15
    putStrLn ""
    printExercise es16 16
    printExercise es17 17
    printExercise es18 18
    printExercise es19 19
    printExercise es20 20
    putStrLn ""
    putStrLn "--------------------Project Euler 21-30--------------"
    printExercise es21 21
    es22 >>= \x -> printExercise x 22
    printExercise es23 23

printExercise :: (Num a, PrintfArg a) => a -> Int -> IO ()
printExercise v esNum = timeItT (printf "Es%d: %d" esNum v) >>= \computation -> printf " (in %.2f) " (fst computation)

printExerciseStr :: String -> Int -> IO ()
printExerciseStr v esNum = timeItT (printf "Es%d: %s" esNum v) >>= \computation -> printf " (in %.2f) " (fst computation)
