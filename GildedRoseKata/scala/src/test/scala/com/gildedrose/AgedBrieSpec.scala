package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class AgedBrieSpec extends Properties("AgedBrie"){

  property("Aged Brie increase in quality the older it gets") = forAll(daysGen, agedBrieGen) {
    (days: Int, agedBrie: Item) => {
      val app = new GildedRose(Array(agedBrie))
        (0 until days).map((_: Int) => {
          val beforeUpdateQuality = agedBrie.quality
          app.updateQuality()
          (agedBrie.quality > beforeUpdateQuality ||
            (agedBrie.quality == 50 && beforeUpdateQuality == 50))
        }).fold(true)(_ && _)
    }
  }
}
