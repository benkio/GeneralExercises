package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def initialItemQualityIncreaseOrDecrease(
    itemQuality: ItemQuality,
    itemName: String,
    itemSellIn: Int
  ): ItemQuality =
    if (!itemName.equals("Aged Brie")
      && !itemName.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemQualityDecrease(itemQuality, itemName)
    } else {

      val itemQualityIncreased: ItemQuality = itemQualityIncrease(itemQuality)

      val itemQualityIncreasedSpecial: ItemQuality = concertSpecialQualityIncrease(itemQualityIncreased, itemName, itemSellIn)

      itemQualityIncreasedSpecial
    }

  def concertSpecialQualityIncrease(
    itemQuality: ItemQuality,
    itemName: String,
    itemSellIn: Int
  ): ItemQuality =
    if (itemQuality.value < 50 && itemName.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemSellIn match {
        case x if x < 6 => itemQualityIncrease(itemQualityIncrease(itemQuality))
        case x if x < 11 => itemQualityIncrease(itemQuality)
        case _ => itemQuality
      }
    } else itemQuality

  def sellInDecrease(itemName: String, itemSellIn: Int): ItemSellIn =
    if (!itemName.equals("Sulfuras, Hand of Ragnaros")) {
       ItemSellIn(itemSellIn - 1)
    } else ItemSellIn(itemSellIn)

  def itemQualityIncrease(itemQuality: ItemQuality): ItemQuality =
    if (itemQuality.value < 50) ItemQuality(itemQuality.value + 1)  else itemQuality

  def itemQualityDecrease(itemQuality: ItemQuality, itemName: String) : ItemQuality =
    if (itemQuality.value > 0 && !itemName.equals("Sulfuras, Hand of Ragnaros")) {
      ItemQuality(itemQuality.value - 1)
    } else itemQuality

  def negativeSellInItemQualityDecrease(itemQuality: ItemQuality, itemName: String): ItemQuality =
    if (!itemName.equals("Backstage passes to a TAFKAL80ETC concert")) {
      itemQualityDecrease(itemQuality, itemName)
    } else ItemQuality()

  def negativeSellInItemQualityDecreaseOrIncrease(itemQuality: ItemQuality, itemName: String): ItemQuality =
    if (!itemName.equals("Aged Brie")) {
      negativeSellInItemQualityDecrease(itemQuality, itemName)
    } else {
      itemQualityIncrease(itemQuality)
    }

  def negativeSellInItemQualityAdjustment(
    itemQuality: ItemQuality,
    itemName: String,
    itemSellIn: ItemSellIn
  ): ItemQuality =
    if (itemSellIn.value < 0) {
      negativeSellInItemQualityDecreaseOrIncrease(itemQuality, itemName)
    } else itemQuality

  def updateQuality() {
    for (i <- 0 until items.length) {
      val itemQualityInitial: ItemQuality = initialItemQualityIncreaseOrDecrease(
        ItemQuality(items(i).quality),
        items(i).name,
        items(i).sellIn
      )

      val itemSellIn: ItemSellIn = sellInDecrease(items(i).name, items(i).sellIn)

      val itemQuality: ItemQuality = negativeSellInItemQualityAdjustment(
        itemQualityInitial,
        items(i).name,
        itemSellIn
      )

      items(i).quality = itemQuality.value
      items(i).sellIn = itemSellIn.value
    }
  }
}
