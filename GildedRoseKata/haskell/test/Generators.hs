module Generators  where

import Test.QuickCheck
import GildedRose
import Test.QuickCheck.Instances.Tuple

itemGen :: String -> Bool -> Gen (Item, Positive Int)
itemGen name sellInExpired = do
  days <- (arbitrary :: Gen (Positive Int))
  negativeSellIn <- (arbitrary :: Gen (Negative Int))
  positiveSellIn <- (arbitrary :: Gen (Positive Int))
  let sellIn = if sellInExpired then (getNegative negativeSellIn) else (getPositive positiveSellIn)
  quality <- choose (0, 50)
  return ((Item name sellIn quality), days)

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
backstagePassesWillExpireGen = do
  days <- choose $ (6, 1000)
  sellIn <- choose $ (0, 5)
  quality <- choose (0, 50)
  return ((Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality), Positive days)

dexterityGen :: Bool -> Gen (Item, Positive Int)
dexterityGen = itemGen "+5 Dexterity Vest"

elixirGen :: Bool -> Gen (Item, Positive Int)
elixirGen = itemGen "Elixir of the Mongoose"

agedBrieGen :: Gen (Item, Positive Int)
agedBrieGen = itemGen "Aged Brie" False

sulfurasGen :: Gen (Item, Positive Int)
sulfurasGen = itemGen "Sulfuras, Hand of Ragnaros" False

allItemGen :: Gen Item
allItemGen = oneof $ fmap (fmap fst) [
  dexterityGen False,
    elixirGen False,
    agedBrieGen,
    sulfurasGen,
    (backstagePassesGen (\_ -> (0, 1000)))
  ]

allItemGenExpired :: Gen Item
allItemGenExpired = oneof $ fmap (fmap fst) [
  dexterityGen True,
    elixirGen True
  ]

allItemsGen :: Gen Item -> Gen ([Item], Positive Int)
allItemsGen x = (listOf x) >*< (arbitrary :: Gen (Positive Int))
