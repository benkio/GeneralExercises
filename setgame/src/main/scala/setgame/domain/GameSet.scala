package setgame.domain

import setgame.rulecheckers.GameSetChecker

case class GameSet(card1: Card, card2: Card, card3: Card)

object GameSet {

  def apply(card1 : Card, card2 : Card, card3 : Card) : Option[GameSet] =
    if (GameSetChecker.check(card1, card2, card3)) Some(new GameSet(card1, card2, card3))
    else None
}
