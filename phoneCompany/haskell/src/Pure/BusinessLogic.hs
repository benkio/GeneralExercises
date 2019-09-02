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
  invoice (OverflowCall _ _ d) = (toEnum . (* (fromEnum overflowRate)) . fromEnum . toSeconds) d
  invoice (StandardRateCall _ _ d) = (toEnum .(* (fromEnum standardRate)) . fromEnum .toSeconds) d

instance Ord Call where
  (<=) c1 c2 = (invoice c1) <= (invoice c2)


-- callToChage :: Call -> Money.Discrete "GBP" "penny"
-- callToChage = undefined

filterLongerCall :: M.Map String [Call] -> M.Map String [Call]
filterLongerCall m = fmap removeMaximum m
                     where
                       removeMaximum :: [Call] -> [Call]
                       removeMaximum cs = let
                         maxElem :: Call
                         maxElem = maximumBy compare cs
                         in filter (\c -> c /= maxElem) cs
