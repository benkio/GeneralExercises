module Lib.Map (updateKeys) where

import qualified Data.Map as M

-- Update the map with the new keys
updateKeys :: Ord k => M.Map k v -> [((k, k), v)] -> M.Map k v
updateKeys originalMap updates = foldl updateMap originalMap updates
  where
    updateMap m ((cOld, cNew), v) =
      let mWithoutOld = M.delete cOld m
      in M.insert cNew v mWithoutOld

