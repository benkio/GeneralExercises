module Pure.Transformation where

import Data.List (filter, groupBy, map, null)
import Data.Map.Strict (Map, fromListWith)
import Pure.Domain

validateInput :: CallLog -> Maybe Call
validateInput cl = do
    called <- number (clCalled cl)
    duration <- parseDuration (clDuration cl)
    return $ call (clCostumerId cl) called duration

groupByCostumer :: [Call] -> Map String [Call]
groupByCostumer calls =
    let inputByCostumer = groupBy (\c1 c2 -> costumerId c1 == costumerId c2) calls
        inputByCostumerFiltered = filter (not . null) inputByCostumer
     in fromListWith (++) $ map (\lc -> ((costumerId . head) lc, lc)) inputByCostumerFiltered
