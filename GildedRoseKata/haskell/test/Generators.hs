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
