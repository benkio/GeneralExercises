module SuccessConditions where

ascendingQualty1 :: (Int, Int) -> Bool
ascendingQualty1 (oldQ, newQ) =
  oldQ < newQ || (oldQ == 50 && newQ == 50)
