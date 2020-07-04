package com.gildedrose

class Item(val name: String, var sellIn: Int, var quality: Int) {

}

case class ItemQuality(value: Int)
case class ItemSellIn(value: Int)

object ItemQuality {
  //def apply(quality: Int): Option[ItemQuality] = ???
  def apply(): ItemQuality = ItemQuality(0)
  def checkRange(itemQuality: ItemQuality): Boolean = (0 until 50) contains itemQuality.value
  def increaseQuality(itemQuality: ItemQuality): ItemQuality =
    if (checkRange(itemQuality)) ItemQuality(itemQuality.value + 1) else itemQuality
  def decreaseQuality(itemQuality: ItemQuality, itemName: ItemName) : ItemQuality = {
    val newQuality = ItemQuality(itemQuality.value - 1)
    if (checkRange(newQuality) && !ItemName.isSulfuras(itemName)) newQuality else itemQuality
  }
}

object ItemName {
  def isSulfuras(itemName: ItemName): Boolean = itemName.equals("Sulfuras, Hand of Ragnaros")
  def isBackstagePasses(itemName: ItemName): Boolean = itemName.equals("Backstage passes to a TAFKAL80ETC concert")
  def isAgedBrie(itemName: ItemName): Boolean = itemName.equals("Aged Brie")
}

object ItemSellIn {
  def isExpired(itemSellIn: ItemSellIn): Boolean = itemSellIn.value < 0
  def decrease(itemSellIn: ItemSellIn, itemName: ItemName): ItemSellIn =
    if (!ItemName.isSulfuras(itemName)) {
       ItemSellIn(itemSellIn.value - 1)
    } else itemSellIn
}
