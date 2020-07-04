package com.gildedrose

class Item(val name: String, var sellIn: Int, var quality: Int) {

}

case class ItemQuality(value: Int)
case class ItemSellIn(value: Int)

object ItemQuality {
  //def apply(quality: Int): Option[ItemQuality] = ???
  def apply(): ItemQuality = ItemQuality(0)
  def checkRange(itemQuality: ItemQuality): Boolean = (0 until 50) contains itemQuality.value
  def increaseQuality(itemQuality: ItemQuality): ItemQuality = ???
  def decreaseQuality(itemQuality: ItemQuality): ItemQuality = ???
}
