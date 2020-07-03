package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class SulfurasSpec extends Properties("Sulfuras"){

  property("Sulfuras Quality never decrese") = forAll(daysGen, sulfurasGen) {
    (days: Int, sulfuras: Item) => {
      val app = new GildedRose(Array(sulfuras))
      val startingQualtity = sulfuras.quality
        (0 until days).map((_: Int) => {
          app.updateQuality()
          sulfuras.quality == startingQualtity
        }).forall(_ == true)
    }
  }
}
