package com.gildedrose

object GildedRose {
  def updateQuality(items: Array[Item]): Array[Item] = for {
    itemRaw <- items
  } yield Item.refine(itemRaw)
    .preSellInQualityAdjustment()
    .sellInDecrease()
    .postSellInQualityAdjustment()
}
