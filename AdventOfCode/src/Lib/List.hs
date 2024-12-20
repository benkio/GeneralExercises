module Lib.List ((\\), find', null', pairsWith, pairs) where

import Data.List (tails)

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) xs c = filter (`notElem` c) xs

find' :: (a -> Bool) -> [a] -> Maybe a
find' p [] = Nothing
find' p (x:xs)
 |p x = Just x
 |otherwise = find' p xs

null' :: [a] -> Bool
null' [] = True
null' _ = False

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

pairsWith :: (a -> a -> c) -> [a] -> [c]
pairsWith f = fmap (uncurry f) . pairs
