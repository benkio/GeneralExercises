module Lib.List ((\\)) where

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) xs c = filter (`notElem` c) xs
