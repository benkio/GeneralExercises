module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

qualityDecrease :: String -> Int -> Int
qualityDecrease name quality =
  if (quality > 0) && (name /= "Sulfuras, Hand of Ragnaros")
  then quality - 1
  else quality

qualityIncrease :: Int -> Int
qualityIncrease quality =
  if quality < 50
  then quality + 1
  else quality

postSellInDecreaseNoBackstageCalculation :: String -> Int -> Int
postSellInDecreaseNoBackstageCalculation name quality =
  if name /= "Backstage passes to a TAFKAL80ETC concert"
  then qualityDecrease name quality
  else 0

postSellInDecreaseNoAgedBrieCalculation :: String -> Int -> Int
postSellInDecreaseNoAgedBrieCalculation name quality = 
  if name /= "Aged Brie"
  then postSellInDecreaseNoBackstageCalculation name quality
  else qualityIncrease quality
  
postSellInDecreaseQualityCalculation :: String -> Int -> Int -> Int
postSellInDecreaseQualityCalculation name sellIn' quality' =
  if sellIn' < 0
  then postSellInDecreaseNoAgedBrieCalculation name quality'
  else quality'

initialQualityCalculation :: String -> Int -> Int -> Int
initialQualityCalculation name sellIn quality
  | name /= "Aged Brie"
      && name /= "Backstage passes to a TAFKAL80ETC concert" =
      qualityDecrease name quality
  | quality < 50 && (name == "Backstage passes to a TAFKAL80ETC concert")
        && ((sellIn < 11) && (quality < 49)) =
    quality + 2 + if (sellIn < 6) && (quality < 48) then 1 else 0
  | quality < 50 =
    quality + 1
      + if (name == "Backstage passes to a TAFKAL80ETC concert")
        && ((sellIn < 11) && (quality < 49))
        then 1 + if (sellIn < 6) && (quality < 48) then 1 else 0
        else 0
  | otherwise = quality

sellInDecrease :: String -> Int -> Int
sellInDecrease name sellIn =
  if name /= "Sulfuras, Hand of Ragnaros"
  then sellIn - 1
  else sellIn

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let quality' = initialQualityCalculation name sellIn quality
          sellIn' = sellInDecrease name sellIn
       in Item name sellIn' $ postSellInDecreaseQualityCalculation name sellIn' quality'
