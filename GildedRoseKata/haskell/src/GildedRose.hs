module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

postSellInExpiredCalculation :: String -> Int -> Int -> Item
postSellInExpiredCalculation name sellIn' quality' =
  if sellIn' < 0
    then
      if name /= "Aged Brie"
        then
          if name /= "Backstage passes to a TAFKAL80ETC concert"
            then
              ( if (quality' > 0) && (name /= "Sulfuras, Hand of Ragnaros")
                  then Item name sellIn' (quality' - 1)
                  else Item name sellIn' quality'
              )
            else Item name sellIn' (quality' - quality')
        else
          if quality' < 50
            then Item name sellIn' (quality' + 1)
            else Item name sellIn' quality'
    else Item name sellIn' quality'

initialQualityCalculation :: String -> Int -> Int -> Int
initialQualityCalculation name sellIn quality
  | name /= "Aged Brie"
      && name /= "Backstage passes to a TAFKAL80ETC concert" =
    if (quality > 0) && (name /= "Sulfuras, Hand of Ragnaros")
      then quality - 1
      else quality
  | quality < 50 =
    quality + 1
      + if (name == "Backstage passes to a TAFKAL80ETC concert")
        && ((sellIn < 11) && (quality < 49))
        then 1 + if (sellIn < 6) && (quality < 48) then 1 else 0
        else 0
  | otherwise = quality

initialSellInCalculation :: String -> Int -> Int
initialSellInCalculation name sellIn =
  if name /= "Sulfuras, Hand of Ragnaros"
    then sellIn - 1
    else sellIn

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let quality' = initialQualityCalculation name sellIn quality
          sellIn' = initialSellInCalculation name sellIn
       in postSellInExpiredCalculation name sellIn' quality'
