{-# LANGUAGE LambdaCase #-}

module TwentyTwentyOne.ThirteenthDecember where

import Control.Monad (mapM_)
import Data.Bifunctor (bimap)
import Data.List (nub, stripPrefix)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M (elems, fromList, keys, lookup, mapKeys, size)
import Data.Maybe (fromJust, isJust)
import Debug.Trace

data Fold = X Int | Y Int deriving (Show)

input :: IO String
input = readFile "input/2021/13December.txt"

parseInput :: String -> (Map (Int, Int) Bool, [Fold])
parseInput =
  bimap
    ( M.fromList
        . fmap
          ( ( \(x, y) ->
                ((read x :: Int, ((\z -> read z :: Int) . tail) y), True)
            )
              . break (== ',')
          )
    )
    ( fmap
        ( ( \case
              ("x", _ : ys) -> X (read ys :: Int)
              ("y", _ : ys) -> Y (read ys :: Int)
              _ -> error "unreachable"
          )
            . break (== '=')
            . fromJust
            . stripPrefix "fold along "
        )
        . tail
    )
    . break null
    . lines

performFold :: Map (Int, Int) Bool -> Fold -> Map (Int, Int) Bool
performFold cs f = M.mapKeys (foldSinglePoint f) cs

foldSinglePoint :: Fold -> (Int, Int) -> (Int, Int)
foldSinglePoint (X foldX) =
  \(x, y) -> if x > foldX then (foldX * 2 - x, y) else (x, y)
foldSinglePoint (Y foldY) =
  \(x, y) -> if y > foldY then (x, foldY * 2 - y) else (x, y)

thirteenthDecemberSolution1 :: IO Int
thirteenthDecemberSolution1 = do
  i <- input
  let (m, fs) = parseInput i
  return $ M.size $ performFold m (head fs)

thirteenthDecemberSolution2 :: IO ()
thirteenthDecemberSolution2 = do
  i <- input
  let (m, fs) = parseInput i
  printManual $ foldl performFold m fs

printManual :: Map (Int, Int) Bool -> IO ()
printManual m =
  let maxX = (maximum . fmap fst . M.keys) m
      maxY = (maximum . fmap snd . M.keys) m
      grid = [if isJust (M.lookup (x, y) m) then '#' else '.' | y <- [0 .. maxY], x <- [0 .. maxX]]
      gridByLine = chunksOf (maxX + 1) grid
   in mapM_ putStrLn gridByLine

inputTest :: String
inputTest =
  "6,10\n\
  \0,14\n\
  \9,10\n\
  \0,3\n\
  \10,4\n\
  \4,11\n\
  \6,0\n\
  \6,12\n\
  \4,1\n\
  \0,13\n\
  \10,12\n\
  \3,4\n\
  \3,0\n\
  \8,4\n\
  \1,10\n\
  \2,14\n\
  \8,10\n\
  \9,0\n\
  \\n\
  \fold along y=7\n\
  \fold along x=5"
