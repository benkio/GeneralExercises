-------------------------------------------------------------------------------
--                           Advent Of Code - day 15                          --
-------------------------------------------------------------------------------
module TwentyTwenty.FifteenthDecember where

import Control.Monad.State
import Data.Bifunctor (second)
import Data.Map (Map, empty, fromList, insertLookupWithKey, lookup)
import Data.Maybe (fromJust, isJust)
import Prelude hiding (lookup)

input :: IO [(Int, Int)]
input =
  (`zip` [1 ..])
    . fmap (\x -> read x :: Int)
    . filter ("" /=)
    . foldl
      ( \acc x ->
          if x == ',' || x == '\n'
            then acc ++ [""]
            else init acc ++ [last acc ++ [x]]
      )
      [""]
    <$> readFile "input/2020/15December.txt"

type SpokenNumbers = Map Int Int

gameSetup :: [(Int, Int)] -> State SpokenNumbers (Int, Int)
gameSetup xs = do
  put $ fromList $ init xs
  return (last xs)

computeTurn :: (Int, Int) -> State SpokenNumbers (Int, Int)
computeTurn (num, prevTurn) = do
  spokenNum <- get
  let (maybeNum, newSpokenNum) =
        insertLookupWithKey
          (\_ currentTurn _ -> currentTurn)
          num
          prevTurn
          spokenNum
      thisTurn = prevTurn + 1
  put newSpokenNum
  return $
    foldr (\oldTurn _ -> (prevTurn - oldTurn, thisTurn)) (0, thisTurn) maybeNum

game :: Int -> [(Int, Int)] -> State SpokenNumbers (Int, Int)
game targetTurn startingNums = do
  (v, t) <- gameSetup startingNums
  gameLoop targetTurn (v, t)

gameLoop :: Int -> (Int, Int) -> State SpokenNumbers (Int, Int)
gameLoop targetTurn (num, prevTurn) = do
  if prevTurn == targetTurn
    then return (num, prevTurn)
    else do
      (v', t') <- computeTurn (num, prevTurn)
      if t' == targetTurn
        then return (v', t')
        else gameLoop targetTurn (v', t')

solutionPattern :: Int -> IO Int
solutionPattern n = fst . (`evalState` empty) . game n <$> input

fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = solutionPattern 2020

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = solutionPattern 30000000
