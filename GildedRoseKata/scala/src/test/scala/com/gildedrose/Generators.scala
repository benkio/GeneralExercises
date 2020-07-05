package com.gildedrose

import org.scalacheck.Gen

object Generators {

  val daysGen: Gen[Int] = Gen.posNum[Int]

  def itemGen(name: String): Gen[Item] = for {
    s <- Gen.posNum[Int]
    q <- Gen.choose(0, 50)
  } yield new Item(name, s, q)

  val sulfurasGen: Gen[Item] = itemGen(Sulfuras.name)
  val agedBrieGen: Gen[Item] = itemGen(AgedBrie.name)
  val elixirGen: Gen[Item] = itemGen("Elixir of the Mongoose")
  val dexterityGen: Gen[Item] = itemGen("+5 Dexterity Vest")

  val backstagePassesFarGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(10, 1000)
    d <- Gen.choose(0, s - 10)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(BackstagePasses.name, s, q))

  val backstagePassesCloseGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(5, 10)
    d <- Gen.choose(0, s - 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(BackstagePasses.name, s, q))

  val backstagePassesClosestGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(0, 5)
    d <- Gen.choose(0, s)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(BackstagePasses.name, s, q))

  val backstagePassesWillExpireGen: Gen[(Int, Item)] = for {
    d <- Gen.choose(6, 1000)
    s <- Gen.choose(0, 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(BackstagePasses.name, s, q))

  val conjuredWillExpireGen: Gen[(Int, Item)] = for {
    d <- Gen.choose(6, 1000)
    s <- Gen.choose(0, 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(Conjured.name, s, q))

  val conjuredNotExpiredGen: Gen[(Int, Item)] = for {
    s <- Gen.choose(6, 1000)
    d <- Gen.choose(0, 5)
    q <- Gen.choose(0, 50)
  } yield (d, new Item(Conjured.name, s, q))

  val conjuredGen: Gen[Item] = Gen.oneOf(
    conjuredWillExpireGen.map(_._2),
    conjuredNotExpiredGen.map(_._2),
  )

  val backstagePassesGen: Gen[Item] = Gen.oneOf(
    backstagePassesFarGen.map(_._2),
    backstagePassesCloseGen.map(_._2),
    backstagePassesClosestGen.map(_._2),
    backstagePassesWillExpireGen.map(_._2)
  )

  val itemGen: Gen[Item] = Gen.oneOf(sulfurasGen, agedBrieGen, elixirGen, dexterityGen, backstagePassesGen, conjuredGen)
  val standardItemGen: Gen[Item] = Gen.oneOf(elixirGen, dexterityGen)
}
