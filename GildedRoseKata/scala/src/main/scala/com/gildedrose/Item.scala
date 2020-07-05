package com.gildedrose

class Item(val name: String, var sellIn: Int, var quality: Int) {

  def preSellInQualityAdjustment(): Item = copyItem(
    quality = ItemQuality.decreaseQuality(
      ItemQuality(quality)
    ).value)
  def postSellInQualityAdjustment(): Item = if (ItemSellIn.isExpired(ItemSellIn(sellIn))) copyItem(quality = ItemQuality.decreaseQuality(ItemQuality(quality)).value)
  else this
  def sellInDecrease(): Item = copyItem(sellIn = ItemSellIn(sellIn - 1).value)

  //Can't touch the Item definition so I manually add the copy method
  def copyItem(sellIn: Int = sellIn, quality: Int = quality): Item = new Item(
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

case class Sulfuras(var sSellIn: Int, var sQuality: Int) extends Item(Sulfuras.name, sSellIn, sQuality) {
  override def preSellInQualityAdjustment(): Sulfuras = this
  override def postSellInQualityAdjustment(): Sulfuras = this
  override def sellInDecrease(): Sulfuras = this
}
object Sulfuras {
  val name: ItemName = "Sulfuras, Hand of Ragnaros"
}
case class BackstagePasses(var bsSellIn: Int, var bsQuality: Int) extends Item(BackstagePasses.name, bsSellIn, bsQuality) {
  override def preSellInQualityAdjustment(): BackstagePasses  = {
    val newQuality = bsSellIn match {
      case x if x < 6 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(bsQuality))))
      case x if x < 11 => ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(bsQuality)))
      case _ => ItemQuality.increaseQuality(ItemQuality(bsQuality))
    }
    this.copy(bsQuality = newQuality.value)
  }
  override def postSellInQualityAdjustment(): BackstagePasses = if (ItemSellIn.isExpired(ItemSellIn(bsSellIn))) this.copy(bsQuality = ItemQuality().value) else this
  override def sellInDecrease(): BackstagePasses = this.copy(bsSellIn = ItemSellIn(bsSellIn - 1).value)
}
object BackstagePasses {
  val name: ItemName = "Backstage passes to a TAFKAL80ETC concert"
}
case class AgedBrie(abSellIn: Int, abQuality: Int) extends Item(AgedBrie.name, abSellIn, abQuality) {
  override def preSellInQualityAdjustment(): AgedBrie  = this.copy(abQuality = ItemQuality.increaseQuality(ItemQuality(abQuality)).value)
  override def postSellInQualityAdjustment(): AgedBrie = this.copy(abQuality = ItemQuality.increaseQuality(ItemQuality(abQuality)).value)
  override def sellInDecrease(): AgedBrie = this.copy(abSellIn = ItemSellIn(abSellIn - 1).value)
}
object AgedBrie {
  val name: ItemName = "Aged Brie"
}
case class Conjured(var cSellIn: Int, var cQuality: Int) extends Item(Conjured.name, cSellIn, cQuality) {
  override def preSellInQualityAdjustment(): Conjured = this.copy(cQuality = ItemQuality.decreaseQuality(ItemQuality.decreaseQuality(ItemQuality(cQuality))).value)
  override def postSellInQualityAdjustment(): Conjured = if (ItemSellIn.isExpired((ItemSellIn(cSellIn))))
    this.copy(cQuality = ItemQuality.increaseQuality(ItemQuality.increaseQuality(ItemQuality(cQuality))).value)
  else this
  override def sellInDecrease(): Conjured = this
}
object Conjured {
  val name: ItemName = "Conjured Mana Cake"
}

object Item {

  def refine(item: Item): Item = item.name match {
    case Sulfuras.name         => new Sulfuras(item.sellIn, item.quality)
    case BackstagePasses.name  => new BackstagePasses(item.sellIn, item.quality)
    case AgedBrie.name         => new AgedBrie(item.sellIn, item.quality)
    case Conjured.name         => new Conjured(item.sellIn, item.quality)
    case _ => item
  }
}
