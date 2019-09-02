{-# LANGUAGE DataKinds #-}

module Pure.BusinessLogic where

import Pure.Domain
import Data.List
import qualified Data.Map.Strict as M
import Money

instance Ord Call where
  (<=) c1 c2 = (duration c1) <= (duration c2)


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
