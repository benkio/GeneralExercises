package domain

import org.scalatest._

class Elements extends FlatSpec with Matchers {

  "createDeck" should "create a deck based on the number of players" in {
    val d : Deck = Deck.createDeck(2)

    d.cards.groupBy(identity).mapValues(_.size).forall(
      { case (_, v) => v == 4 }
    ) should be (true)

    val d2 : Deck = Deck.createDeck(4)

    d2.cards.groupBy(identity).mapValues(_.size).forall(
      { case (_, v) => v == 8 }
    ) should be (true)
  }

  "createDeck" should "be shuffled" in {
    val d : Deck = Deck.createDeck(2)
    d.cards.map(c => c match {
                  case King()       => 13
                  case Queen()      => 12
                  case Jack()       => 11
                  case Ace()        => 1
                  case valueCard(v) => v
                }) should not be sorted
  }

  "getCard" should "return a card and the rest of the deck" in {
    val d = Deck.createDeck(0)
    val (card, d1) = Deck.getCard(d)

    card shouldBe a [Card]
    d1.cards shouldEqual (d.cards.tail)
    d1.cards.size shouldEqual 51
    val (card3, d4) = Deck.getCard(d1)
    d4.cards.size shouldEqual 50
    card should not equal card3

    val d2 = Deck(List())
    val (card2, d3) = Deck.getCard(d2)

    card2 shouldBe a [Card]
    d3.cards.size shouldEqual 51

  }

  "Card" should "recognize king, queen,jack and ace" in {
    Card(13)  shouldBe a [King]
    Card(12) shouldBe a [Queen]
    Card(11) shouldBe a [Jack]
    Card(1) shouldBe a [Ace]
    Card(5) shouldBe a [valueCard]

    an [Exception] should be thrownBy Card(25)
  }

  "Player" should "be able to split in case of equal cards" in {
    val p = Player(Wait, Right(List(Card(1), Card(1))), 10)
    p.canSplit shouldBe true

    val p1 = Player(Wait, Right(List(Card(1), Card(1), Card(1))), 10)
    p1.canSplit should not be true

    val p2 = Player(Wait, Right(List(Card(5), Card(7), Card(1))), 10)
    p2.canSplit should not be true
  }

  "Player" should "be able to bet " in {
    val p = Player(Wait, Right(List(Card(1), Card(1))), 10)
    p.canBet(5) shouldBe true
    p.canBet(12) should not be true
    p.canBet(-3) shouldBe false
  }

  "Player" should "change its status when stand" in {
    val p = Player(Play, Right(List(Card(1), Card(1))), 10)
    val result = p.stand
    result.status shouldBe Wait
    result.cards shouldEqual (p.cards)
    result.amount shouldEqual (p.amount)
  }

  "Player" should "be able to hit" in {
    val p1 = Player(Play, Right(List(Card(1), Card(1))), 10)
    val result1 = p1.hit(Card(5), false)
    result1.status shouldBe Play
    result1.cards should be (Right(List(Card(1), Card(1), Card(5))))
    result1.amount shouldEqual (p1.amount)

    val p2 = Player(Play, Left((List(Card(1)), List(Card(5)))), 10)
    val result2 = p2.hit(Card(3), false)
    result2.status shouldBe Play
    result2.cards should be (Left((List(Card(1), Card(3)), List(Card(5)))))
    result2.amount shouldEqual (p2.amount)

    val p3 = Player(Play, Left((List(Card(1)), List(Card(5)))), 10)
    val result3 = p3.hit(Card(3), true)
    result3.status shouldBe Play
    result3.cards should be (Left((List(Card(1)), List(Card(5), Card(3)))))
    result3.amount shouldEqual (p3.amount)
  }

  "Player" should "split, if possible" in {
    val p1 = Player(Play, Right(List(Card(1), Card(1))), 10)
    val result = p1.split
    result.status shouldBe Play
    result.cards should be (Left((List(Card(1)), List(Card(1)))))
    result.amount shouldEqual (p1.amount)

    val p2 = Player(Play, Right(List(Card(2), Card(1))), 10)
    val result1 = p2.split
    result1 shouldEqual (p2)
  }

  "Player" should "bet, if possible" in {
    val p = Player(Wait, Right(List(Card(1), Card(1))), 10)
    p.bet(5).amount shouldBe 5
    p.bet(32) shouldEqual (p)
  }
}