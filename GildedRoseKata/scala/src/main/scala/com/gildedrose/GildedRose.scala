package com.gildedrose

class GildedRose(val items: Array[Item]) {

  def initialItemQualityIncreaseOrDecrease(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: Int
  ): ItemQuality =
    if (!ItemName.isAgedBrie(itemName)
      && !ItemName.isBackstagePasses(itemName)) {
      ItemQuality.decreaseQuality(itemQuality, itemName)
    } else {

      val itemQualityIncreased: ItemQuality = ItemQuality.increaseQuality(itemQuality)

      val itemQualityIncreasedSpecial: ItemQuality = concertSpecialQualityIncrease(itemQualityIncreased, itemName, itemSellIn)

      itemQualityIncreasedSpecial
    }

  def concertSpecialQualityIncrease(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: Int
  ): ItemQuality =
    if (ItemName.isBackstagePasses(itemName)) {
      itemSellIn match {
        case x if x < 6 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(itemQuality))
        case x if x < 11 => ItemQuality.increaseQuality(itemQuality)
        case _ => itemQuality
      }
    } else itemQuality

  def negativeSellInItemQualityAdjustment(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: ItemSellIn
  ): ItemQuality = (ItemSellIn.isExpired(itemSellIn), !ItemName.isAgedBrie(itemName), !ItemName.isBackstagePasses(itemName)) match {
    case (true, true, true) => ItemQuality.decreaseQuality(itemQuality, itemName)
    case (true, true, false) => ItemQuality()
    case (true, false, _) =>  ItemQuality.increaseQuality(itemQuality)
    case (false, _, _) => itemQuality
  }

  def updateQuality() {
    for (i <- 0 until items.length) {
      val itemQualityInitial: ItemQuality = initialItemQualityIncreaseOrDecrease(
        ItemQuality(items(i).quality),
        items(i).name,
        items(i).sellIn
      )

      val itemSellIn: ItemSellIn = ItemSellIn.decrease(
        ItemSellIn(items(i).sellIn),
        items(i).name
      )

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
