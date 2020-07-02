package com.gildedrose

import org.scalacheck.Gen

object Generators {

  val daysGen: Gen[Int] = Gen.posNum[Int]
  val sulfurasGen: Gen[Item] = for {
    s <- Gen.posNum[Int]
    q <- Gen.choose(0, 50)
  } yield new Item("Sulfuras, Hand of Ragnaros", s, q)

  val agedBrieGen: Gen[Item] = for {
    s <- Gen.posNum[Int]
    q <- Gen.choose(0, 50)
  } yield new Item("Aged Brie", s, q)

  val backstagePassesFarGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(10, 1000)
    d <- Gen.choose(0, s - 10)
    q <- Gen.choose(0, 50)
  } yield (d, new Item("Backstage passes to a TAFKAL80ETC concert", s, q))

  val backstagePassesCloseGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(5, 10)
    d <- Gen.choose(0, s - 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item("Backstage passes to a TAFKAL80ETC concert", s, q))

  val backstagePassesClosestGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(0, 5)
    d <- Gen.choose(0, s)
    q <- Gen.choose(0, 50)
  } yield (d, new Item("Backstage passes to a TAFKAL80ETC concert", s, q))

  val backstagePassesWillExireGen: Gen[(Int, Item)] = for {
    d <- Gen.choose(6, 1000)
    s <- Gen.choose(0, 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item("Backstage passes to a TAFKAL80ETC concert", s, q))
}
