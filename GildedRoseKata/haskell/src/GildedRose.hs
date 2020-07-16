module GildedRose where

import Item

itemRefinement :: Item -> AllItems
itemRefinement (Item "Sulfuras, Hand of Ragnaros" s q) = SpecialItem $ Sulfuras (SellIn s) $ Quality q
itemRefinement (Item "Aged Brie" s q) = SpecialItem $ AgedBrie (SellIn s) $ Quality q
itemRefinement (Item "Backstage passes to a TAFKAL80ETC concert" s q) = SpecialItem $ BackstagePasses (SellIn s) $ Quality q
itemRefinement i@Item{} = StandardItem i

itemWiden :: AllItems -> Item
itemWiden (SpecialItem (Sulfuras (SellIn s) (Quality q))) = Item "Sulfuras, Hand of Ragnaros" s q
itemWiden (SpecialItem (AgedBrie (SellIn s) (Quality q))) = Item "Aged Brie" s q
itemWiden (SpecialItem (BackstagePasses (SellIn s) (Quality q))) = Item "Backstage passes to a TAFKAL80ETC concert" s q
itemWiden (StandardItem i) = i

updateQuality :: GildedRose -> GildedRose
updateQuality = map (itemWiden . calcQ . calcSellIn . itemRefinement)
