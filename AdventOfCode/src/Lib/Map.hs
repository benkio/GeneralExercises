module Lib.Map (updateKeys) where

import Lib.Coord (Coord)

import qualified Data.Map as M

-- Update the map with the new keys
updateKeys :: (Ord k) => M.Map k v -> [((k, k), v)] -> M.Map k v
updateKeys = foldl updateMap
  where
    updateMap m ((cOld, cNew), v) =
        let mWithoutOld = M.delete cOld m
         in M.insert cNew v mWithoutOld
