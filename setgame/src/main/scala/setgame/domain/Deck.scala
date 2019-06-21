package setgame.domain

import scala.util.Random

case class Deck private(cards: Set[Card])

object Deck {

  def apply() : Deck = new Deck(
    Random.shuffle(
      for {
        sp <- Shape.values
        c  <- Color.values
        n  <- Number.values
        sd <- Shading.values
      } yield Card(sp, c, n, sd)).toSet
  )
}
