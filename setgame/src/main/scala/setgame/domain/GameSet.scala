package setgame.domain

import setgame.rulecheckers.GameSetChecker

case class GameSet(card1: Card, card2: Card, card3: Card)

object GameSet {

  def apply(cards : Seq[Card]) : Option[GameSet] =
    if (cards.size == 3 &&
      GameSetChecker.check(cards(0), cards(1), cards(2)))
      Some(new GameSet(cards(0), cards(1), cards(2)))
    else None
}
