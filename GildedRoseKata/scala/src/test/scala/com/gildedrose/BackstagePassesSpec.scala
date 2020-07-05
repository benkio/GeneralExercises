package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class BackstagePassesSpec extends Properties("BackstagePasses"){

  property("Backstage Passes increase in quality by one if expiration is > 10 days away") = forAll(backstagePassesFarGen) {
    case (days: Int, backstagePassesFar: Item) => {
      (0 until days).map((_: Int) => {
          val beforeUpdateQuality = backstagePassesFar.quality
          val result = GildedRose.updateQuality(Array(backstagePassesFar))
          (result.length == 1 && (result(0).quality == (beforeUpdateQuality + 1) ||
            (result(0).quality == 50 && beforeUpdateQuality == 50)))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes increase in quality by two if expiration is between 5-10 days away") = forAll(backstagePassesCloseGen) {
    case (days: Int, backstagePassesClose: Item) => {
      (0 until days).map((d: Int) => {
          val beforeUpdateQuality = backstagePassesClose.quality
          val result = GildedRose.updateQuality(Array(backstagePassesClose))
          (result.length == 1 && (result(0).quality == (beforeUpdateQuality + 2) ||
            (result(0).quality == 50 && beforeUpdateQuality == 49) ||
            (result(0).quality == 50 && beforeUpdateQuality == 50)))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes increase in quality by three if expiration is between 0-5 days away") = forAll(backstagePassesClosestGen) {
    case (days: Int, backstagePassesClosest: Item) => {
      (0 until days).map((d: Int) => {
          val beforeUpdateQuality = backstagePassesClosest.quality
          val result = GildedRose.updateQuality(Array(backstagePassesClosest))
          (result.length == 1 && (result(0).quality == (beforeUpdateQuality + 3) ||
            (result(0).quality == 50 && beforeUpdateQuality == 48) ||
            (result(0).quality == 50 && beforeUpdateQuality == 49) ||
            (result(0).quality == 50 && beforeUpdateQuality == 50)))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes drop quality to 0 if expiration happens") = forAll(backstagePassesWillExireGen) {
    case (days: Int, backstagePassesWillExpire: Item) => {
      (0 until days).map((d: Int) => {
          val result = GildedRose.updateQuality(Array(backstagePassesWillExpire))
          (result.length == 1 && result(0).quality == 0)
        }).dropWhile(_ == false).forall(_ == true)
    }
  }
}
