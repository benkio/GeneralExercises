package com.gildedrose

class Item(val name: String, var sellIn: Int, var quality: Int) {

  def preSellInQualityAdjustment(): Item = copy(
    quality = ItemQuality.decreaseQuality(
      ItemQuality(quality)
    ).value)
  def postSellInQualityAdjustment(): Item = if (ItemSellIn.isExpired(ItemSellIn(sellIn))) copy(quality = ItemQuality.decreaseQuality(ItemQuality(quality)).value)
  else this
  def sellInDecrease(): Item = copy(sellIn = ItemSellIn(sellIn - 1).value)

  //Can't touch the Item definition so I manually add the copy method
  def copy(sellIn: Int = sellIn, quality: Int = quality): Item = new Item(
    this.name,
    sellIn,
    quality
  )
}

case class ItemQuality(value: Int) extends AnyVal
case class ItemSellIn(value: Int) extends AnyVal

object ItemQuality {
  def apply(): ItemQuality = ItemQuality(0)
  def checkRange(itemQuality: ItemQuality): Boolean = (0 until 50) contains itemQuality.value
  def increaseQuality(itemQuality: ItemQuality): ItemQuality =
    if (checkRange(itemQuality)) ItemQuality(itemQuality.value + 1) else itemQuality
  def decreaseQuality(itemQuality: ItemQuality) : ItemQuality = {
    val newQuality = ItemQuality(itemQuality.value - 1)
    if (checkRange(newQuality)) newQuality else itemQuality
  }
}

object ItemName {
  def isSulfuras(itemName: ItemName): Boolean = itemName.equals(Sulfuras.name)
  def isBackstagePasses(itemName: ItemName): Boolean = itemName.equals(BackstagePasses.name)
  def isAgedBrie(itemName: ItemName): Boolean = itemName.equals(AgedBrie.name)
}

object ItemSellIn {
  def isExpired(itemSellIn: ItemSellIn): Boolean = itemSellIn.value < 0
}

class Sulfuras(sellIn: Int, quality: Int) extends Item(Sulfuras.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): Item = this
  override def postSellInQualityAdjustment(): Item = this
  override def sellInDecrease(): Item = this
}
object Sulfuras {
  val name: ItemName = "Sulfuras, Hand of Ragnaros"
}
class BackstagePasses(sellIn: Int, quality: Int) extends Item(BackstagePasses.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): Item  = {
    val newQuality = sellIn match {
      case x if x < 6 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(quality))))
      case x if x < 11 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(quality)))
      case _ => ItemQuality.increaseQuality(ItemQuality(quality))
    }
    this.copy(quality = newQuality.value)
  }
  override def postSellInQualityAdjustment(): Item = this.copy(quality = ItemQuality().value)
}
object BackstagePasses {
  val name: ItemName = "Backstage passes to a TAFKAL80ETC concert"
}
class AgedBrie(sellIn: Int, quality: Int) extends Item(AgedBrie.name, sellIn, quality) {
  override def preSellInQualityAdjustment(): Item  = this.copy(quality = ItemQuality.increaseQuality(ItemQuality(quality)).value)
  override def postSellInQualityAdjustment(): Item = this.copy(quality = ItemQuality.increaseQuality(ItemQuality(quality)).value)
}
object AgedBrie {
  val name: ItemName = "Aged Brie"
}

object Item {

  def refine(item: Item): Item = item.name match {
    case Sulfuras.name         => new Sulfuras(item.sellIn, item.quality)
    case BackstagePasses.name  => new BackstagePasses(item.sellIn, item.quality)
    case AgedBrie.name         => new AgedBrie(item.sellIn, item.quality)
    case _ => item
  }
}
