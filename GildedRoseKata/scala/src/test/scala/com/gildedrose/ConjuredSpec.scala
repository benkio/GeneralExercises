package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class ConjuredSpec extends Properties("ConjuredSpec"){

  property("Conjured decrese in quality twice as fast when the sellIn is not expired") = forAll(conjuredNotExpiredGen) {
    case (days: Int, conjured: Item) => {
      (0 until days).foldLeft((true, conjured)) {
        case ((acc: Boolean, item: Item), _: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc &&
            result.length == 1 &&
            result(0).quality == (item.quality - 2) ||
            (result(0).quality == 0 && item.quality == 1) ||
            (result(0).quality == 0 && item.quality == 0))
            (resultCheck, result(0))
          }}._1
      }
    }

      property("Conjured decrese in quality four time as fast when the sellIn is not expired") = forAll(conjuredWillExpireGen) {
        case (days: Int, conjured: Item) => {
          (0 until days).foldLeft((true, conjured)) {
            case ((acc: Boolean, item: Item), _: Int) => {
              val result = GildedRose.updateQuality(Array(item))
              val resultCheck = item.sellIn >= 0 || (acc &&
                result.length == 1 &&
                result(0).quality == (item.quality - 4) ||
                (result(0).quality == 0 && item.quality == 3) ||
                (result(0).quality == 0 && item.quality == 2) ||
                (result(0).quality == 0 && item.quality == 1) ||
                (result(0).quality == 0 && item.quality == 0))
                (resultCheck, result(0))
              }}._1
          }
        }
      }
