module Item
  (
  GildedRose,
  ItemName,
  AllItems(..),
  Item(..),
  SpecialItem(..),
  Quality(..),
  HasQuality(..),
  HasSellIn(..),
  HasName(..),
  SellIn(..)) where

-- Types ----------------------------------------------------------------------

type GildedRose = [Item]
type ItemName = String

data AllItems = StandardItem Item | SpecialItem SpecialItem
data Item = Item ItemName Int Int
  deriving (Eq)
data SpecialItem =
    Sulfuras SellIn Quality
  | AgedBrie SellIn Quality
  | BackstagePasses SellIn Quality
  | Conjured SellIn Quality

newtype Quality = Quality { valueQ :: Int } deriving (Show)
newtype SellIn = SellIn { valueS :: Int } deriving (Show)

-- Typeclass ------------------------------------------------------------------

class HasQuality a where
  calcQ :: a -> a
  getQuality :: a -> Quality

class HasName a where
  getName :: a -> ItemName

class HasSellIn a where
  calcSellIn :: a -> a
  getSellIn :: a -> SellIn

-- instances ------------------------------------------------------------------

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

instance Show SpecialItem where
  show i@(Sulfuras sellIn quality) =
    getName i ++ ", " ++ show sellIn ++ ", " ++ show quality
  show i@(AgedBrie sellIn quality) =
    getName i ++ ", " ++ show sellIn ++ ", " ++ show quality
  show i@(BackstagePasses sellIn quality) =
    getName i ++ ", " ++ show sellIn ++ ", " ++ show quality
  show i@(Conjured sellIn quality) =
    getName i ++ ", " ++ show sellIn ++ ", " ++ show quality

instance Show AllItems where
  show (StandardItem i) = "standard :" ++ show i
  show (SpecialItem i) = "special  :" ++ show i

instance HasQuality AllItems where
  calcQ (StandardItem i) = StandardItem $ calcQ i
  calcQ (SpecialItem i) = SpecialItem $ calcQ i
  getQuality (StandardItem i) = getQuality i
  getQuality (SpecialItem i) = getQuality i

instance HasQuality Item where
  calcQ (Item n s q) =
    Item n s (valueQ (qualityDecrease (SellIn s) (Quality q) (\x -> if valueS x < 0 then 2 else 1)))
  getQuality (Item _ _ q) = Quality q

instance HasQuality SpecialItem where
  getQuality (Sulfuras _ q) = q
  getQuality (AgedBrie _ q) = q
  getQuality (Conjured _ q) = q
  getQuality (BackstagePasses _ q) = q
  calcQ i@(Sulfuras _ _) = i
  calcQ (Conjured s q) = Conjured s (qualityDecrease s q (const 2))
  calcQ (AgedBrie s q)
    | isExpired s = AgedBrie s $ (qualityIncrease . qualityIncrease) q
    | otherwise = AgedBrie s $ qualityIncrease q
  calcQ (BackstagePasses s q)
    | isExpired s = BackstagePasses s $ Quality 0
    | (valueS s < 6) && (valueQ q < 48) = BackstagePasses s $ iterate qualityIncrease q !! 2
    | (valueS s < 11) && (valueQ q < 49) = BackstagePasses s $ iterate qualityIncrease q !! 1
    | otherwise = BackstagePasses s $ qualityIncrease q

instance HasName Item where
  getName (Item m _ _) = m

instance HasName SpecialItem where
  getName Sulfuras{} = "Sulfuras, Hand of Ragnaros"
  getName Conjured{} = "Conjured Mana Cake"
  getName AgedBrie{} = "Aged Brie"
  getName BackstagePasses{} = "Backstage passes to a TAFKAL80ETC concert"

instance HasName AllItems where
  getName i@StandardItem{} = getName i
  getName i@SpecialItem{} = getName i

instance HasSellIn Item where
  calcSellIn (Item m s q) = Item m (s - 1) q
  getSellIn (Item _ s _) = SellIn s

instance HasSellIn SpecialItem where
  getSellIn (Sulfuras s _) = s
  getSellIn (Conjured s _) = s
  getSellIn (AgedBrie s _) = s
  getSellIn (BackstagePasses s _) = s
  calcSellIn (Sulfuras s q) = Sulfuras s q
  calcSellIn (Conjured s q) = Conjured (SellIn (valueS s - 1)) q
  calcSellIn (AgedBrie s q) = AgedBrie (SellIn (valueS s - 1)) q
  calcSellIn (BackstagePasses s q) = BackstagePasses (SellIn (valueS s - 1)) q

instance HasSellIn AllItems where
  getSellIn (StandardItem i) = getSellIn i
  getSellIn (SpecialItem i) = getSellIn i
  calcSellIn (StandardItem i) = StandardItem $ calcSellIn i
  calcSellIn (SpecialItem i) = SpecialItem $ calcSellIn i

-- functions -----------------------------------

isValid :: Quality -> Bool
isValid q = valueQ q `elem` [0..50]

isExpired :: SellIn -> Bool
isExpired (SellIn s) = s < 0

qualityIncrease :: Quality -> Quality
qualityIncrease quality =
  if isValid q' then q' else quality
  where q' = Quality (valueQ quality + 1)

qualityDecrease :: SellIn -> Quality -> (SellIn -> Int) -> Quality
qualityDecrease s q decreaseCoeff =
  if isValid q' then q' else q
  where
    q' = Quality $ max (valueQ q - decreaseCoeff s) 0
