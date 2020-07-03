package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._
import org.scalacheck.Gen

class QualitySpec extends Properties("QualitySpec") {

  property("Quality never exceed 50") = forAll(daysGen, Gen.listOf[Item](itemGen)) {
    (days: Int, itemList: List[Item]) => {
      val items: Array[Item] = itemList.toArray
      val app = new GildedRose(items)
        (0 until days).map((_: Int) => {
          app.updateQuality()
          items.forall(_.quality <= 50)
        }).forall(_ == true)
    }
  }

  property("Quality is never negative") = forAll(daysGen, Gen.listOf[Item](itemGen)) {
    (days: Int, itemList: List[Item]) => {
      val items: Array[Item] = itemList.toArray
      val app = new GildedRose(items)
        (0 until days).map((_: Int) => {
          app.updateQuality()
          items.forall(_.quality >= 0)
        }).forall(_ == true)
    }
  }

  property("Quality decrease twice as fast once the sell by date has passed") = forAll(daysGen, Gen.listOf[Item](standardItemGen)) {
    (days: Int, itemList: List[Item]) => {
      val items: Array[Item] = itemList.toArray
      val app = new GildedRose(items)
        (0 until days).map((_: Int) => {
          val expiredItemQualities =
            items
              .zipWithIndex
              .filter(_._1.sellIn <= 0)
              .map{ case (i, index) => (i.quality, index) }
          val notExpiredItemQualities =
            items
              .zipWithIndex
              .filter(_._1.sellIn > 0)
              .map{ case (i, index) => (i.quality, index) }

          app.updateQuality()

          expiredItemQualities.map {
            case (qualityBefore, index) =>
              items(index).quality == (qualityBefore - 2) ||
              (items(index).quality == 0 && qualityBefore == 1) ||
              (items(index).quality == 0 && qualityBefore == 0)
          }.forall(_ == true) &&
          notExpiredItemQualities.map {
            case (qualityBefore, index) =>
              items(index).quality == (qualityBefore - 1) ||
              (items(index).quality == 0 && qualityBefore == 0)
          }.forall(_ == true)
        }).forall(_ == true)
    }
  }
}
