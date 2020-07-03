package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class ConjuredSpec extends Properties("ConjuredSpec"){

  // TODO: uncomment after the feature is done
  // property("Conjured decrese in quality twice as fast") = forAll(daysGen, conjuredGen) {
  //   (days: Int, conjured: Item) => {
  //     val app = new GildedRose(Array(conjured))
  //       (0 until days).map((_: Int) => {
  //         val beforeUpdateQuality = conjured.quality
  //         app.updateQuality()
  //         (conjured.quality == (beforeUpdateQuality - 2) ||
  //           (conjured.quality == 0 && beforeUpdateQuality == 1))
  //       }).forall(_ == true)
  //   }
  // }
}
