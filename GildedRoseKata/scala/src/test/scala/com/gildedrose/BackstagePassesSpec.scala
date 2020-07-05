package com.gildedrose

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import Generators._

class BackstagePassesSpec extends Properties("BackstagePasses"){

  property("Backstage Passes increase in quality by one if expiration is > 10 days away") = forAll(backstagePassesFarGen) {
    case (days: Int, backstagePassesFar: Item) => {
      (0 until days).foldLeft((true, backstagePassesFar)){
        case((acc: Boolean, item: Item),_: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc && result.length == 1 && (result(0).quality == (item.quality + 1) ||
            (result(0).quality == 50 && item.quality == 50)))
          (resultCheck, result(0))
        }}._1
    }
  }

  property("Backstage Passes increase in quality by two if expiration is between 5-10 days away") = forAll(backstagePassesCloseGen) {
    case (days: Int, backstagePassesClose: Item) => {
      (0 until days).foldLeft((true, backstagePassesClose)){
        case((acc: Boolean, item: Item),d: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc && result.length == 1 &&
            (result(0).quality == (item.quality + 2) ||
            (result(0).quality == 50 && item.quality == 49) ||
            (result(0).quality == 50 && item.quality == 50)))
          (resultCheck, result(0))
        }}._1
    }
  }

  property("Backstage Passes increase in quality by three if expiration is between 0-5 days away") = forAll(backstagePassesClosestGen) {
    case (days: Int, backstagePassesClosest: Item) => {
      (0 until days).foldLeft((true, backstagePassesClosest)){
        case((acc: Boolean, item: Item),d: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc && result.length == 1 && (result(0).quality == (item.quality + 3) ||
            (result(0).quality == 50 && item.quality == 48) ||
            (result(0).quality == 50 && item.quality == 49) ||
            (result(0).quality == 50 && item.quality == 50)))
          (resultCheck, result(0))
        }}._1
    }
  }

  property("Backstage Passes drop quality to 0 if expiration happens") = forAll(backstagePassesWillExireGen) {
    case (days: Int, backstagePassesWillExpire: Item) => {
      (0 until days).foldLeft((true, backstagePassesWillExpire)){
        case((acc: Boolean, item: Item),d: Int) => {
          val result = GildedRose.updateQuality(Array(item))
          val resultCheck = (acc && result.length == 1 &&
            (
              (result(0).quality == 0 && result(0).sellIn < 0) ||
              result(0).sellIn >= 0
            ))
          (resultCheck, result(0))
      }}._1
    }
  }
}
