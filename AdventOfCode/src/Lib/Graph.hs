module Lib.Graph (
    fromConnectionList,
    nextPaths,
    uniquePathOfLength,
    uniqueConnectedPathOfLength,
) where

import Data.Functor ((<&>))
import Data.Map (Map, alter, empty, fromList, (!?))
import Data.Maybe (fromMaybe)
import Lib.List ((\\))

-- Connections are bidirectional
fromConnectionList :: (Ord a) => [(a, a)] -> Map a [a]
fromConnectionList =
    foldl go empty
  where
    go m (k, v) = alter (updateGraph k) v $ alter (updateGraph v) k m
    updateGraph v a = case (a <&> \x -> if v `elem` x then x else v : x) of
        Nothing -> Just [v]
        x -> x

nextPaths :: (Ord a) => [a] -> Map a [a] -> [[a]]
nextPaths [] _ = []
nextPaths xs m = next <&> \x -> xs ++ [x]
  where
    next = fromMaybe [] $ m !? (last xs) <&> \x -> x \\ (init xs)

uniquePathOfLength :: (Ord a) => Int -> [[a]] -> Map a [a] -> [[a]]
uniquePathOfLength n xss m = pathOfLength n xss isUniquePath m

uniqueConnectedPathOfLength :: (Ord a) => Int -> [[a]] -> Map a [a] -> [[a]]
uniqueConnectedPathOfLength n xss m = pathOfLength n xss f m
  where
    f xs xss = xs `isUniquePath` xss && areAllConnected m xs

pathOfLength :: (Ord a) => Int -> [[a]] -> ([a] -> [[a]] -> Bool) -> Map a [a] -> [[a]]
pathOfLength _ [] _ _ = []
pathOfLength n xss f m
    | n <= (length . head) xss = xss
    | otherwise = pathOfLength n xss' f m
  where
    xss' = foldl next [] xss
    next acc xs = acc ++ filter (`f` acc) (nextPaths xs m)

isUniquePath :: (Eq a) => [a] -> [[a]] -> Bool
isUniquePath xs [] = True
isUniquePath xs ns = all (not . isEqual xs) ns

isEqual :: (Eq a) => [a] -> [a] -> Bool
isEqual xs ys =
    length xs == length ys
        && all (`elem` ys) xs
        && all (`elem` xs) ys

areAllConnected :: (Ord a) => Map a [a] -> [a] -> Bool
areAllConnected m [] = True
areAllConnected m [a] = True
areAllConnected m (x : xs) = checkConnection && areAllConnected m xs
  where
    checkConnection = fromMaybe False $ m !? x <&> \ys -> all (`elem` ys) xs
