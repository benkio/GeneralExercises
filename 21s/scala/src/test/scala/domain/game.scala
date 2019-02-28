package domain

import org.scalatest._
import domain._

class GameTest extends FlatSpec with Matchers {

  "PointCalculator.calculatePoints" should "calculate the right amount of points" in {
    val d = List(Card(12), Card(13), Card(11), Card(1), Card(5))
    PointCalculator.calculatePoints(d) shouldEqual 22

    val c = List(Card(1), Card(2), Card(5), Card(7))
    PointCalculator.calculatePoints(c) shouldEqual 25

    val e = List(Card(10), Card(10))
    PointCalculator.calculatePoints(e) shouldEqual 20

    val f = List()
    PointCalculator.calculatePoints(f) shouldEqual 0
  }

  "PointCalculator.calculatePoint" should "calculate the right amount of point for single card" in {
    val c = Card(1)
    val c1 = Card(11)
    val c2 = Card(12)
    val c3 = Card(13)
    val c4 = Card(5)

    PointCalculator.calculatePoint(c) shouldEqual 11
    PointCalculator.calculatePoint(c1) shouldEqual 1
    PointCalculator.calculatePoint(c2) shouldEqual 2
    PointCalculator.calculatePoint(c3) shouldEqual 3
    PointCalculator.calculatePoint(c4) shouldEqual 5
  }

  "checkPlayerPoints" should "bust a player" in {
    val p = Player(Play, Right(List(Card(10), Card(10), Card(1))), 10)
    val (e, p1) = PointCalculator.checkPlayerPoints(p)

    e should be (Right(31))
    (p1.status) shouldEqual Bust
  }

  "checkPlayerPoints" should "bust a split player" in {
    val p = Player(Play,
                   Left((List(Card(1)), List(Card(10), Card(10), Card(1)))),
                   10)
    val (e, p1)  = PointCalculator.checkPlayerPoints(p)
    e should be (Right(11))
    p1.status shouldEqual Play
    p1.cards should be (Right(List(Card(1))))


    val p2 = Player(Play,
                   Left((List(Card(10), Card(10), Card(1)), List(Card(1)))),
                   10)
    val (e2, p3) = PointCalculator.checkPlayerPoints(p2)
    e2 shouldEqual (Right(11))
    p3.status shouldEqual Play
    p3.cards should be (Right(List(Card(1))))
  }

  "Bank" should "end play if the intermediate player status is in wait or Bust" in {
    val result = Game.playerEndPlay(Player(Wait, Right( List() ), 10))
    val result2 = Game.playerEndPlay(Player(Bust, Right( List() ), 10))
    val result3 = Game.playerEndPlay(Player(Play, Right( List() ), 10))

    result shouldEqual true
    result2 shouldEqual true
    result3 shouldEqual false
  }

  "Bank" should "perform the expected action" in {
    val deck = Deck(List(Card(5), Card(1), Card(10), Card(2)))
    val player = Player(Play, Right(List()), 10)

    val (d, p) = Bank.performAction(deck, player)

    d shouldEqual (Deck(List(Card(1), Card(10), Card(2))))
    p.status shouldEqual Play
    p.cards shouldEqual Right(List(Card(5)))
    p.amount shouldEqual 10

    val (d1, p1) = Bank.performAction(d, p)

    d1 shouldEqual (Deck(List(Card(10), Card(2))))
    p1.status shouldEqual Play
    p1.cards shouldEqual Right(List(Card(5), Card(1)))
    p1.amount shouldEqual 10

    val (d2, p2) = Bank.performAction(d1, p1)

    d2 shouldEqual (Deck(List(Card(2))))
    p2.status shouldEqual Bust
    p2.cards shouldEqual Right(List(Card(5), Card(1), Card(10)))
    p2.amount shouldEqual 10
  }

  "Bank" should "play until burst or get to the threshold" in {
    val deck = Deck(List(Card(10), Card(5), Card(2), Card(2)))
    val (d, p) = Bank.play(deck)

    d shouldEqual (Deck(List(Card(2))))
    p.status shouldEqual Wait
    p.cards shouldEqual Right(List(Card(10), Card(5), Card(2)))

    val deck2 = Deck(List(Card(10), Card(5), Card(1), Card(2)))
    val (d1, p1) = Bank.play(deck2)

    d1 shouldEqual (Deck(List(Card(2))))
    p1.cards shouldEqual Right(List(Card(10), Card(5), Card(1)))
    p1.status shouldEqual Bust
  }

  "distributeOneCardToEveryone" should "return a new list of players, each with one card" in {
    val deck = Deck.createDeck(1)
    val lps = List(Player(Play, Right(List()),10),
                   Player(Play, Left((List(), List())), 10),
                   Player(Play, Left(List(), List()), 10))
    val (d, result) = Game.distributeOneCardToEveryone(deck, lps)

    d.cards.size shouldEqual (deck.cards.size - 3)
    result.foreach(p => p.cards match {
                     case Right(l) => l.size shouldEqual 1
                     case Left((l1, l2)) => {
                       l1.size shouldEqual 1
                       l2.size shouldEqual 1
                     }
                   })
  }

  "setupPlayers" should "return a new list of players with a specific template" in {
    val result = Game.setupPlayers(2)
    result.foreach(p => p shouldEqual Player(Wait,
                                             Right(List()),
                                             GameConstraints.startingAmount))
  }

  "WinCondition" should "find if a player wins or not" in {
    val p1 = Player(Wait, Right(List(Card(10), Card(11), Card(8))), 70)
    val p2 = Player(Wait,
                    Left(List(Card(4), Card(7), Card(12), Card(5)),
                         List(Card(4), Card(7), Card(6))),
                    75)

    val bank = Player(Wait,
                      Right(List(Card(8), Card(13), Card(6))),
                      Int.MaxValue)

    val result1 = Game.winCordition(bank, p1)
    val result2 = Game.winCordition(bank, p2)

    result1 shouldEqual true
    result2 shouldEqual true
  }
}