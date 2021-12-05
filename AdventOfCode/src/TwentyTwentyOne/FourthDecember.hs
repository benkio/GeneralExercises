module TwentyTwentyOne.FourthDecember where

import Data.List (find, groupBy, transpose, (\\))
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Debug.Trace

type Grid = [[(Int, Bool)]]

input :: IO String
input = readFile "input/2021/4December.txt"

testInput :: String
testInput =
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
  \\n\
  \22 13 17 11  0\n\
  \ 8  2 23  4 24\n\
  \21  9 14 16  7\n\
  \ 6 10  3 18  5\n\
  \ 1 12 20 15 19\n\
  \\n\
  \ 3 15  0  2 22\n\
  \ 9 18 13 17  5\n\
  \19  8  7 25 23\n\
  \20 11 10 24  4\n\
  \14 21 16 12  6\n\
  \\n\
  \14 21 17 24  4\n\
  \10 16 15  9 19\n\
  \18  8 23 26 20\n\
  \22 11 13  6  5\n\
  \ 2  0 12  3  7"

parseInput :: String -> ([Int], [Grid])
parseInput s =
  let ls = lines s
      extraction = (fmap ((\x -> (read x :: Int)) . T.unpack) . T.splitOn (T.pack ",") . T.pack . head) ls
      grids = (fmap (fmap (fmap (\x -> (read x :: Int, False)) . words)) . filter ((== 5) . length) . groupBy (\l l' -> not (null l || null l')) . drop 2) ls
   in (extraction, grids)

checkWin :: Grid -> Bool
checkWin g =
  let col = transpose g
   in any (all snd) col || any (all snd) g

gameLoop :: [Int] -> [Grid] -> (Int, Grid)
gameLoop ns [] = error $ "no boards" ++ show ns
gameLoop [] gs = error $ "no more numbers " ++ show gs
gameLoop (n : ns) gs =
  let gs' = fmap (setNumber n) gs
      winners = find checkWin gs'
   in if isJust winners then (n, fromJust winners) else gameLoop ns gs'

setNumber :: Int -> Grid -> Grid
setNumber n = fmap (fmap (\(n', v) -> if n' == n then (n', True) else (n', v)))

calculateScore :: (Int, Grid) -> Int
calculateScore (n, g) = n * (sum . fmap fst . filter (not . snd) . concat) g

fourthDecemberSolution1 :: IO Int
fourthDecemberSolution1 = calculateScore . uncurry gameLoop . parseInput <$> input

gameLoop' :: [Int] -> [Grid] -> (Int, Grid)
gameLoop' ns [] = error $ "no boards" ++ show ns
gameLoop' [] gs = error $ "no more numbers " ++ show gs
gameLoop' (n : ns) gs =
  let gs' = fmap (setNumber n) gs
      rest = filter (not . checkWin) gs'
      winner = find checkWin gs'
   in if null rest && isJust winner then (n, fromJust winner) else gameLoop' ns rest

fourthDecemberSolution2 :: IO Int
fourthDecemberSolution2 = calculateScore . uncurry gameLoop' . parseInput <$> input
