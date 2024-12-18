module Lib.Map (updateKeys,updateLowestScore) where

import Lib.Coord (Coord, cardinalNeighboors, findCardinalNeighboors)

import qualified Data.Map as M

-- Update the map with the new keys
updateKeys :: Ord k => M.Map k v -> [((k, k), v)] -> M.Map k v
updateKeys originalMap updates = foldl updateMap originalMap updates
  where
    updateMap m ((cOld, cNew), v) =
      let mWithoutOld = M.delete cOld m
      in M.insert cNew v mWithoutOld

updateLowestScore :: Coord -> Int -> M.Map Coord Int -> M.Map Coord Int
updateLowestScore c v =
    M.alter (\mv -> maybe (Just v) (Just . min v) mv) c
