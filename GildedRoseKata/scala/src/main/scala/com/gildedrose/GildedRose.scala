package com.gildedrose

object GildedRose {

  def initialItemQualityIncreaseOrDecrease(
    itemQuality: ItemQuality,
    itemName: ItemName,
    itemSellIn: Int
  ): ItemQuality = (
    (!ItemName.isAgedBrie(itemName) && !ItemName.isBackstagePasses(itemName)),
    ItemName.isBackstagePasses(itemName),
    itemSellIn) match {
    case (true, _, _) => ItemQuality.decreaseQuality(itemQuality, itemName)
    case (false, true ,x) if x < 6 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality.increaseQuality(itemQuality)))
    case (false, true, x) if x < 11 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(itemQuality))
    case _ => ItemQuality.increaseQuality(itemQuality)
  }

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

  def updateQuality(items: Array[Item]): Array[Item] = for {
      item <- items
      initialQuality = initialItemQualityIncreaseOrDecrease(ItemQuality(item.quality), item.name, item.sellIn)
      itemSellIn = ItemSellIn.decrease(ItemSellIn(item.sellIn), item.name)
      resultQuality = negativeSellInItemQualityAdjustment(initialQuality, item.name, itemSellIn)
    } yield new Item(item.name, itemSellIn.value, resultQuality.value)
}
