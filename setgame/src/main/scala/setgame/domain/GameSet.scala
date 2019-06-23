package setgame.domain

import setgame.rulecheckers.GameSetChecker

case class GameSet(card1: Card, card2: Card, card3: Card)

object GameSet {

  def apply(cards : Set[Card]) : Option[GameSet] = {
    val cardsSeq : Seq[Card] = cards.toSeq
    if (cards.size == 3 &&
      GameSetChecker.check(cardsSeq(0), cardsSeq(1), cardsSeq(2)))
      Some(new GameSet(cardsSeq(0), cardsSeq(1), cardsSeq(2)))
    else None
  }
}
