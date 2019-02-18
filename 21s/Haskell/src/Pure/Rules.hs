module Pure.Rules
  (
    getHandValue,
    hasBlackjack,
    hasMoreThen17,
    hasLost,
    score
  )
where

import Pure.Domain

getHandValue :: [Card] -> Int
getHandValue hs = foldr (\c v -> v + (to21Value (cValue c))) 0 hs

hasBlackjack :: Player -> Bool
hasBlackjack = (==21) . score

hasMoreThen17 :: Player -> Bool
hasMoreThen17 = (>17) . score

hasLost :: Player -> Bool
hasLost = (>21) . score

score :: Player -> Int
score = getHandValue . hand

instance Ord Player where
  compare (Player {hand=h}) (Player {hand=h'})
    | (getHandValue h) > (getHandValue h') = GT
    | (getHandValue h) < (getHandValue h') = LT
    | otherwise = EQ