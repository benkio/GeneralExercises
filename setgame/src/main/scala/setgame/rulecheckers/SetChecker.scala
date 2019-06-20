package setgame.rulecheckers

import setgame.domain._

object SetChecker extends Checker[Card] {

  def check(card1: Card, card2 : Card, card3: Card) : Boolean =
    ShapeChecker.check(card1.shape, card2.shape, card3.shape) &&
    ColorChecker.check(card1.color, card2.color, card3.color) &&
    NumberChecker.check(card1.number, card2.number, card3.number) &&
    ShadingChecker.check(card1.shading, card2.shading, card3.shading)

}
