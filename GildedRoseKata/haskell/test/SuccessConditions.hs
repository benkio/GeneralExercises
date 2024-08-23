module SuccessConditions where

ascendingQualty :: Int -> (Int, Int) -> Bool
ascendingQualty distance (oldQ, newQ) =
    let upperBoundExceptions = [(50 - distance + 1) .. 50]
        exceptionConditions = foldl (\acc x -> acc || (oldQ == x && newQ == 50)) True upperBoundExceptions
     in oldQ == (newQ - distance) || exceptionConditions

ascendingQualty1 :: (Int, Int) -> Bool
ascendingQualty1 = ascendingQualty 1

ascendingQualty2 :: (Int, Int) -> Bool
ascendingQualty2 = ascendingQualty 2

ascendingQualty3 :: (Int, Int) -> Bool
ascendingQualty3 = ascendingQualty 3

qualityDegradesExpired :: (Int, Int) -> Bool
qualityDegradesExpired (oldQ, newQ) = newQ == (oldQ - 2) || (oldQ == 1 && newQ == 0) || (oldQ == 0 && newQ == 0)

qualityNeverNegative :: Int -> Bool
qualityNeverNegative = (>= 0)

qualityLessThen50 :: Int -> Bool
qualityLessThen50 = (<= 50)
