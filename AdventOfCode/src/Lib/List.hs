module Lib.List (
    (\\),
    find',
    null',
    pairsWith,
    pairs,
    rotate,
    prependToLists,
    filterByShortLength,
    filterByMostConsecutiveEqElems
) where

import Data.List (tails, groupBy, sortOn, group)
import Data.Maybe (listToMaybe, fromMaybe)

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) xs c = filter (`notElem` c) xs

find' :: (a -> Bool) -> [a] -> Maybe a
find' p [] = Nothing
find' p (x : xs)
    | p x = Just x
    | otherwise = find' p xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

pairsWith :: (a -> a -> c) -> [a] -> [c]
pairsWith f = fmap (uncurry f) . pairs

rotate :: Int -> [a] -> [a]
rotate times xs = take (length xs) . drop times . cycle $ xs

-- WARNING: very slow for big lists, use Seq could be better, but still...
prependToLists :: [[a]] -> [[a]] -> [[a]]
prependToLists [] yss = yss
prependToLists xss [] = xss
prependToLists xss yss = do
  ys <- yss
  xs <- xss
  return $ xs ++ ys

filterByShortLength :: [[a]] -> [[a]]
filterByShortLength = fromMaybe [] . listToMaybe . groupBy (\x y -> length x == length y) . sortOn length

filterByMostConsecutiveEqElems :: (Eq a, Ord a) => [[a]] -> [[a]]
filterByMostConsecutiveEqElems = fromMaybe [] . listToMaybe . groupBy (\x y -> nonConsecutiveEqElems x == nonConsecutiveEqElems y) . sortOn nonConsecutiveEqElems
  where
    nonConsecutiveEqElems :: Ord a => [a] -> Int
    nonConsecutiveEqElems = length . filter (==1) . fmap length . group
