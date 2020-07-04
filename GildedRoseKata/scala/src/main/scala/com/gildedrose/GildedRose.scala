package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def initialItemQualityIncreaseOrDecrease(item: Item): Item =
    if (!item.name.equals("Aged Brie")
      && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemQualityDecrease(item)
    } else {

      itemQualityIncrease(item)

      concertSpecialQualityIncrease(item)

      item
    }

  def concertSpecialQualityIncrease(item: Item): Item =
    if (item.quality < 50 && item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (item.sellIn < 11) {
        itemQualityIncrease(item)
      }

      if (item.sellIn < 6) {
        itemQualityIncrease(item)
      }
      item
    } else item

  def sellInDecrease(item: Item): Item =
    if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
      item.sellIn = item.sellIn - 1
      item
    } else item

  def itemQualityIncrease(item: Item): Item =
    if (item.quality < 50) {
      item.quality = item.quality + 1
      item
    } else item

  def itemQualityDecrease(item: Item) =
    if (item.quality > 0 && !item.name.equals("Sulfuras, Hand of Ragnaros")) {
      item.quality = item.quality - 1
      item
    } else item

  def negativeSellInItemQualityAdjustment(item: Item): Item =
    if (item.sellIn < 0) {
      if (!item.name.equals("Aged Brie")) {
        if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          itemQualityDecrease(item)
        } else {
          item.quality = 0
        }
      } else {
        itemQualityIncrease(item)
      }
      item
    } else item

  def updateQuality() {
    for (i <- 0 until items.length) {
      val item1: Item = initialItemQualityIncreaseOrDecrease(items(i))

      val item2: Item = sellInDecrease(item1)

      val item3: Item = negativeSellInItemQualityAdjustment(item2)

      items(i) = item3
    }
  }
}
