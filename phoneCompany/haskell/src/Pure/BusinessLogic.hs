{-# LANGUAGE DataKinds #-}

module Pure.BusinessLogic where

import Data.Foldable
import Data.Hourglass
import Data.List
import qualified Data.Map.Strict as M
import Money
import Pure.Domain

class Invoice a where
    invoice :: a -> Money.Discrete "GBP" "penny"

instance Invoice Call where
    invoice (StandardRateCall _ _ d) = (toEnum . (* (fromEnum standardRate)) . fromEnum . toSeconds) d
    invoice (OverflowCall _ _ d) =
        let
            standardRateDurationInSeconds = (fromEnum . toSeconds) standardRateDuration -- should be 180 (3 minutes)
            overflowInvoice =
                ( (* (fromEnum overflowRate))
                    . (\x -> x - standardRateDurationInSeconds)
                    . fromEnum
                    . toSeconds
                )
                    d
         in
            toEnum $ overflowInvoice + (standardRateDurationInSeconds * (fromEnum standardRate))

instance Ord Call where
    (<=) c1 c2 = (invoice c1) <= (invoice c2)

removeMaximum :: [Call] -> [Call]
removeMaximum cs =
    let maxElem = maximumBy compare cs
     in filter (\c -> c /= maxElem) cs

filterLongerCall :: M.Map String [Call] -> M.Map String [Call]
filterLongerCall m = fmap removeMaximum m

calculateCostumerInvoice :: M.Map String [Call] -> M.Map String (Money.Discrete "GBP" "penny")
calculateCostumerInvoice = fmap (foldl (+) (Money.discrete 0) . fmap (invoice))

businessLogic :: M.Map String [Call] -> M.Map String (Money.Discrete "GBP" "penny")
businessLogic = calculateCostumerInvoice . filterLongerCall
