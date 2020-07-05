package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class AgedBrieSpec extends Properties("AgedBrie"){

  property("Aged Brie increase in quality the older it gets") = forAll(daysGen, agedBrieGen) {
    (days: Int, agedBrie: Item) => {
      (0 until days).foldLeft((true, agedBrie)) {
        case ((acc: Boolean, item: Item), _: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc &&
            result.length == 1 &&
            (result(0).quality > item.quality ||
              (result(0).quality == 50 && item.quality == 50)))
          (resultCheck, result(0))
        }}._1
    }
  }
}
