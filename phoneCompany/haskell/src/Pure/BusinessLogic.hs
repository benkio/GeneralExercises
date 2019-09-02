{-# LANGUAGE DataKinds #-}

module Pure.BusinessLogic where

import Pure.Domain
import Data.List
import qualified Data.Map.Strict as M
import Money
import Data.Hourglass

class Invoice a where
  invoice :: a -> Money.Discrete "GBP" "penny"

instance Invoice Call where
  invoice (StandardRateCall _ _ d) = (toEnum .(* (fromEnum standardRate)) . fromEnum .toSeconds) d
  invoice (OverflowCall _ _ d) =
    let
      standardRateDurationInSeconds = (fromEnum . toSeconds) standardRateDuration --should be 180 (3 minutes)
      overflowInvoice = ((* (fromEnum overflowRate)) .
                         (\x -> x - standardRateDurationInSeconds) .
                         fromEnum .
                         toSeconds) d
    in toEnum $ overflowInvoice + (standardRateDurationInSeconds * (fromEnum standardRate))

instance Ord Call where
  (<=) c1 c2 = (invoice c1) <= (invoice c2)

filterLongerCall :: M.Map String [Call] -> M.Map String [Call]
filterLongerCall m = fmap removeMaximum m
                     where
                       removeMaximum :: [Call] -> [Call]
                       removeMaximum cs = let
                         maxElem :: Call
                         maxElem = maximumBy compare cs
                         in filter (\c -> c /= maxElem) cs
