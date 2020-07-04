package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def initialItemQualityIncreaseOrDecrease(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: Int
  ): ItemQuality =
    if (!ItemName.isAgedBrie(itemName)
      && !ItemName.isBackstagePasses(itemName)) {
      itemQualityDecrease(itemQuality, itemName)
    } else {

      val itemQualityIncreased: ItemQuality = itemQualityIncrease(itemQuality)

      val itemQualityIncreasedSpecial: ItemQuality = concertSpecialQualityIncrease(itemQualityIncreased, itemName, itemSellIn)

      itemQualityIncreasedSpecial
    }

  def concertSpecialQualityIncrease(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: Int
  ): ItemQuality =
    if (itemQuality.value < 50 && ItemName.isBackstagePasses(itemName)) {
      itemSellIn match {
        case x if x < 6 => itemQualityIncrease(itemQualityIncrease(itemQuality))
        case x if x < 11 => itemQualityIncrease(itemQuality)
        case _ => itemQuality
      }
    } else itemQuality

  def sellInDecrease(itemName: ItemName, itemSellIn: Int): ItemSellIn =
    if (!ItemName.isSulfuras(itemName)) {
       ItemSellIn(itemSellIn - 1)
    } else ItemSellIn(itemSellIn)

  def itemQualityIncrease(itemQuality: ItemQuality): ItemQuality =
    if (itemQuality.value < 50) ItemQuality(itemQuality.value + 1)  else itemQuality

  def itemQualityDecrease(itemQuality: ItemQuality, itemName: ItemName) : ItemQuality =
    if (itemQuality.value > 0 && !ItemName.isSulfuras(itemName)) {
      ItemQuality(itemQuality.value - 1)
    } else itemQuality

  def negativeSellInItemQualityDecrease(itemQuality: ItemQuality, itemName: ItemName): ItemQuality =
    if (!ItemName.isBackstagePasses(itemName)) {
      itemQualityDecrease(itemQuality, itemName)
    } else ItemQuality()

  def negativeSellInItemQualityDecreaseOrIncrease(itemQuality: ItemQuality, itemName: ItemName): ItemQuality =
    if (!ItemName.isAgedBrie(itemName)) {
      negativeSellInItemQualityDecrease(itemQuality, itemName)
    } else {
      itemQualityIncrease(itemQuality)
    }

  def negativeSellInItemQualityAdjustment(
    itemQuality: ItemQuality,
    itemName: ItemName,
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
