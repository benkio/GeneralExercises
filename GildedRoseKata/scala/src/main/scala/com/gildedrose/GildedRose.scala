package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def initialItemQualityIncreaseOrDecrease(item: Item): Item =
    if (!item.name.equals("Aged Brie")
      && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemQualityDecrease(item)
    } else {

      val itemQualityIncreased = itemQualityIncrease(item)

      val itemQualityIncreasedSpecial = concertSpecialQualityIncrease(itemQualityIncreased)

      itemQualityIncreasedSpecial
    }

  def concertSpecialQualityIncrease(item: Item): Item =
    if (item.quality < 50 && item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      item.sellIn match {
        case x if x < 6 => itemQualityIncrease(itemQualityIncrease(item))
        case x if x < 11 => itemQualityIncrease(item)
        case _ => item
      }
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

  def negativeSellInItemQualityDecrease(item: Item): Item =
    if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemQualityDecrease(item)
      item
    } else {
      item.quality = 0
      item
    }
  def negativeSellInItemQualityDecreaseOrIncrease(item: Item): Item =
    if (!item.name.equals("Aged Brie")) {
      negativeSellInItemQualityDecrease(item)
    } else {
      itemQualityIncrease(item)
}

  def negativeSellInItemQualityAdjustment(item: Item): Item =
    if (item.sellIn < 0) {
      negativeSellInItemQualityDecreaseOrIncrease(item)
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
