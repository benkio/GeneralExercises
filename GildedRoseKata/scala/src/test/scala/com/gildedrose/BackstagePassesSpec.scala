package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class BackstagePassesSpec extends Properties("BackstagePasses"){

  property("Backstage Passes increase in quality by one if expiration is > 10 days away") = forAll(backstagePassesFarGen) {
    case (days: Int, backstagePassesFar: Item) => {
      val app = new GildedRose(Array(backstagePassesFar))
        (0 until days).map((_: Int) => {
          val beforeUpdateQuality = backstagePassesFar.quality
          app.updateQuality()
          (backstagePassesFar.quality == (beforeUpdateQuality + 1) ||
            (backstagePassesFar.quality == 50 && beforeUpdateQuality == 50))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes increase in quality by two if expiration is between 5-10 days away") = forAll(backstagePassesCloseGen) {
    case (days: Int, backstagePassesClose: Item) => {
      val app = new GildedRose(Array(backstagePassesClose))
        (0 until days).map((d: Int) => {
          val beforeUpdateQuality = backstagePassesClose.quality
          app.updateQuality()
          (backstagePassesClose.quality == (beforeUpdateQuality + 2) ||
            (backstagePassesClose.quality == 50 && beforeUpdateQuality == 49) ||
            (backstagePassesClose.quality == 50 && beforeUpdateQuality == 50))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes increase in quality by three if expiration is between 0-5 days away") = forAll(backstagePassesClosestGen) {
    case (days: Int, backstagePassesClosest: Item) => {
      val app = new GildedRose(Array(backstagePassesClosest))
        (0 until days).map((d: Int) => {
          val beforeUpdateQuality = backstagePassesClosest.quality
          app.updateQuality()
          (backstagePassesClosest.quality == (beforeUpdateQuality + 3) ||
            (backstagePassesClosest.quality == 50 && beforeUpdateQuality == 48) ||
            (backstagePassesClosest.quality == 50 && beforeUpdateQuality == 49) ||
            (backstagePassesClosest.quality == 50 && beforeUpdateQuality == 50))
        }).forall(_ == true)
    }
  }

  property("Backstage Passes drop quality to 0 if expiration happens") = forAll(backstagePassesWillExireGen) {
    case (days: Int, backstagePassesWillExpire: Item) => {
      val app = new GildedRose(Array(backstagePassesWillExpire))
        (0 until days).map((d: Int) => {
          app.updateQuality()
          backstagePassesWillExpire.quality == 0
        }).dropWhile(_ == false).forall(_ == true)
    }
  }
}
