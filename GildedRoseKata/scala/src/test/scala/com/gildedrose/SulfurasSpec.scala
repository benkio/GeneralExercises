package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class SulfurasSpec extends Properties("Sulfuras"){

  property("Sulfuras Quality never decrese") = forAll(daysGen, sulfurasGen) {
    (days: Int, sulfuras: Item) => {
      val startingQualtity = sulfuras.quality
        (0 until days).foldLeft((true, sulfuras)){
          case ((acc: Boolean, item: Item), _: Int) => {
            val result = GildedRose.updateQuality(Array(item))
            val resultCheck = (acc && result.length == 1 && result(0).quality == startingQualtity)
            (resultCheck, result(0))
          }}._1
        }
    }
  }
