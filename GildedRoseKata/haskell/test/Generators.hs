module Generators  where

import Test.QuickCheck.Gen
import Test.QuickCheck
import GildedRose

itemGen :: String -> Gen (Item, Positive Int)
itemGen name = do
  days <- (arbitrary :: Gen (Positive Int))
  sellIn <- (arbitrary :: Gen (Positive Int))
  quality <- choose (0, 50)
  return ((Item name (getPositive sellIn) quality), days)

getQuality :: Item -> Int
getQuality (Item _ _ q) = q

getSellIn :: Item -> Int
getSellIn (Item _ s _) = s

backstagePassesGen :: (Int -> (Int, Int)) -> Gen (Item, Positive Int)
backstagePassesGen sellInBoundsFunc = do
  days <- (arbitrary :: Gen (Positive Int))
  sellIn <- choose $ sellInBoundsFunc (getPositive days)
  quality <- choose (0, 50)
  return ((Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality), days)

backstagePassesFarGen :: Gen (Item, Positive Int)
backstagePassesFarGen = backstagePassesGen $ \days -> (days + 10, maxBound :: Int)

backstagePassesCloseGen :: Gen (Item, Positive Int)
backstagePassesCloseGen = backstagePassesGen $ \days -> (days + 5, days + 10)

backstagePassesClosestGen :: Gen (Item, Positive Int)
backstagePassesClosestGen = backstagePassesGen $ \days -> (days, days + 5)

backstagePassesWillExpireGen :: Gen (Item, Positive Int)
backstagePassesWillExpireGen = backstagePassesGen $ \days -> (0, days - 1)
