module Lib.List ((\\), find') where

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) xs c = filter (`notElem` c) xs

find' :: (a -> Bool) -> [a] -> Maybe a
find' p [] = Nothing
find' p (x:xs)
 |p x = Just x
 |otherwise = find' p xs
