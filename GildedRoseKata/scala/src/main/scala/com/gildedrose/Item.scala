package com.gildedrose

class Item(val name: String, var sellIn: Int, var quality: Int){

  def preSellInQualityAdjustment(): ItemQuality = ItemQuality.decreaseQuality(
    ItemQuality(quality),
    name
  )
  def postSellInQualityAdjustment(): ItemQuality =
    if (ItemSellIn.isExpired(ItemSellIn(sellIn))) ItemQuality.decreaseQuality(ItemQuality(quality), name)
    else ItemQuality(quality)
}

case class ItemQuality(value: Int) extends AnyVal
case class ItemSellIn(value: Int) extends AnyVal

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
  def isSulfuras(itemName: ItemName): Boolean = itemName.equals(Sulfuras.name)
  def isBackstagePasses(itemName: ItemName): Boolean = itemName.equals(BackstagePasses.name)
  def isAgedBrie(itemName: ItemName): Boolean = itemName.equals(AgedBrie.name)
}

object ItemSellIn {
  def isExpired(itemSellIn: ItemSellIn): Boolean = itemSellIn.value < 0
  def decrease(itemSellIn: ItemSellIn, itemName: ItemName): ItemSellIn =
    if (!ItemName.isSulfuras(itemName)) {
      ItemSellIn(itemSellIn.value - 1)
    } else itemSellIn
}

class Sulfuras(sellIn: Int, quality: Int) extends Item(Sulfuras.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): ItemQuality = ItemQuality(quality)
  override def postSellInQualityAdjustment(): ItemQuality = ItemQuality(quality)
}
object Sulfuras {
  val name: ItemName = "Sulfuras, Hand of Ragnaros"
}
class BackstagePasses(sellIn: Int, quality: Int) extends Item(BackstagePasses.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): ItemQuality  = sellIn match {
    case x if x < 6 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(quality))))
    case x if x < 11 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(quality)))
    case _ => ItemQuality.increaseQuality(ItemQuality(quality))
  }
  override def postSellInQualityAdjustment(): ItemQuality = ItemQuality()
}
object BackstagePasses {
  val name: ItemName = "Backstage passes to a TAFKAL80ETC concert"
}
class AgedBrie(sellIn: Int, quality: Int) extends Item(AgedBrie.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): ItemQuality  = ItemQuality.increaseQuality(ItemQuality(quality))
  override def postSellInQualityAdjustment(): ItemQuality = ItemQuality.increaseQuality(ItemQuality(quality))
}
object AgedBrie {
  val name: ItemName = "Aged Brie"
}
