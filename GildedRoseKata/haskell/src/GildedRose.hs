module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

data SpecialItem =
    Sulfuras Int Quality
  | AgedBrie Int Quality
  | BackstagePasses Int Quality

newtype Quality = Quality { valueQ :: Int }

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

class HasQuality a where
  calcQ :: a -> a

isValid :: Quality -> Bool
isValid q = valueQ q `elem` [0..50]

instance HasQuality Item where
  calcQ i@(Item n s q) =
    if isValid q' then Item n s (valueQ q') else i
    where q' = Quality (q - 1)

instance HasQuality SpecialItem where
   calcQ i@(Sulfuras _ _) = i
   calcQ (AgedBrie s q)
     | s < 0 = AgedBrie s $ (qualityIncrease . qualityIncrease) q
     | otherwise = AgedBrie s $ qualityIncrease q
   calcQ (BackstagePasses s q)
     | s < 0 = BackstagePasses s $ Quality 0
     | (s < 6) && (valueQ q < 48) = BackstagePasses s $ iterate qualityIncrease q !! 2
     | (s < 11) && (valueQ q < 49) = BackstagePasses s $ iterate qualityIncrease q !! 1
     | otherwise = BackstagePasses s $ qualityIncrease q

-- TODO remove and use the typeclass
qualityDecrease :: String -> Quality -> Quality
qualityDecrease name quality =
  if isValid q' && (name /= "Sulfuras, Hand of Ragnaros") then q' else quality
  where q' = Quality (valueQ quality - 1)

-- TODO remove and use the typeclass
qualityIncrease :: Quality -> Quality
qualityIncrease quality =
  if isValid q' then q' else quality
  where q' = Quality (valueQ quality + 1)

postSellInDecreaseQualityCalculation :: String -> Int -> Quality -> Quality
postSellInDecreaseQualityCalculation name sellIn' quality
  | sellIn' < 0 && name /= "Aged Brie" && name /= "Backstage passes to a TAFKAL80ETC concert" = qualityDecrease name quality
  | sellIn' < 0 && name == "Backstage passes to a TAFKAL80ETC concert" = Quality 0
  | sellIn' < 0 && name == "Aged Brie" = qualityIncrease quality
  | otherwise = quality

initialQualityCalculation :: String -> Int -> Quality -> Quality
initialQualityCalculation name sellIn quality
  | name /= "Aged Brie"
      && name /= "Backstage passes to a TAFKAL80ETC concert" =
      qualityDecrease name quality
  | isValid quality && (name == "Backstage passes to a TAFKAL80ETC concert")
        && (sellIn < 6) && (valueQ quality < 48) = iterate qualityIncrease quality !! 2
  | isValid quality && (name == "Backstage passes to a TAFKAL80ETC concert")
        && (sellIn < 11) && (valueQ quality < 49) = iterate qualityIncrease quality !! 1
  | isValid quality = qualityIncrease quality
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
      let quality' = initialQualityCalculation name sellIn $ Quality quality
          sellIn' = sellInDecrease name sellIn
       in Item name sellIn' $ valueQ $ postSellInDecreaseQualityCalculation name sellIn' quality'
