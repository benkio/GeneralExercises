module Lib.Graph (
    fromConnectionList,
    nextPaths,
    uniquePathOfLength,
    uniqueConnectedPathOfLength,
    longestPath,
    longestConnectedPath,
)
where

import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.List (maximumBy, sort)
import Data.Map (Map, alter, delete, empty, fromList, insert, (!?))
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Debug.Trace
import Lib.List ((\\))

-- Connections are bidirectional
fromConnectionList :: (Ord a) => [(a, a)] -> Map a [a]
fromConnectionList =
    foldl go empty
  where
    go m (k, v) = alter (updateGraph k) v $ alter (updateGraph v) k m
    updateGraph v a = case a <&> \x -> if v `elem` x then x else v : x of
        Nothing -> Just [v]
        x -> x

nextPaths :: (Ord a) => [a] -> Map a [a] -> [[a]]
nextPaths [] _ = []
nextPaths xs m = next <&> \x -> xs ++ [x]
  where
    next = maybe [] (\x -> x \\ init xs) (m !? last xs)

uniquePathOfLength :: (Ord a) => Int -> [[a]] -> Map a [a] -> [[a]]
uniquePathOfLength n xss = pathOfLength n xss isUniquePath

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
isUniquePath xs ns = (not . any (isEqual xs)) ns

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
    checkConnection = maybe False (\ys -> all (`elem` ys) xs) (m !? x)

longestConnectedPath :: (Ord a, Show a) => [a] -> Map a [a] -> Map [a] [a] -> Map [a] [a]
longestConnectedPath as = longestPath as areAllConnected

longestPath :: (Ord a, Show a) => [a] -> (Map a [a] -> [a] -> Bool) -> Map a [a] -> Map [a] [a] -> Map [a] [a]
longestPath [] _ _ r = r
longestPath as f m r =
    if null ns
        then insert (sort as) (sort as) r
        else foldl foldLongest r ns
  where
    ns = filter (f m) $ nextPaths as m
    alterLongest n x Nothing =
        Just x
    alterLongest n x (Just y) =
        (Just . sort . maximumBy (comparing length)) [x, y]
    foldLongest r' n =
        let r'' = longestPath (sort n) f m r'
            longestN = r' !? sort n
            longestNr = r'' !? sort n
         in maybe
                (alter (alterLongest as (fromJust longestNr)) as r'')
                (\l -> alter (alterLongest as l) as r')
                longestN
