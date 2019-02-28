package domain

sealed trait Card
case class King()  extends Card {
  override def toString() : String = {
    "| K |"
  }
}
case class Queen() extends Card {
  override def toString() : String = {
    "| Q |"
  }
}
case class Jack()  extends Card {
  override def toString() : String = {
    "| J |"
  }
}
case class Ace()   extends Card {
  override def toString() : String = {
    "| A |"
  }
}
case class valueCard(value : Int) extends Card {
  override def toString() : String = {
    "| " + value + " |"
  }
}

object Card {
  def apply(value : Int) = value match {
    case 13 => King()
    case 12 => Queen()
    case 11 => Jack()
    case 1  => Ace()
    case x if x < 13 && x > 0 => valueCard(x)
    case default => throw new Exception("Value of new card exceed allowed range")
  }
}

case class Deck(cards : List[Card])
object Deck {
  def createDeck(numberOfPlayers : Int) : Deck = {
    val numberOfDecks : Int =
      if (Math.ceil(numberOfPlayers / 3.0).toInt == 0) 1
      else Math.ceil(numberOfPlayers / 3.0).toInt
    val deck : List[Card] = List.fill(4)(for (
      a <- (1 to 13)
    ) yield Card(a)).flatten
    Deck(scala.util.Random.shuffle(List.fill(numberOfDecks)(deck).flatten))
  }

  def getCard(deck : Deck) : (Card, Deck) = deck.cards match {
    case c :: (tail : List[Card]) => (c, Deck(tail))
    case Nil       => getCard(createDeck(0))
  }
}

case class Player (val status : PlayerStatus,
                   val cards  : Either[(List[Card], List[Card]), List[Card]],
                   val amount : Int) {

  def canSplit : Boolean = {
    val c = cards.getOrElse(List())
    c.size == 2 && c.forall(_ == c.head)
  }

  def canBet(bet : Int) : Boolean = amount > bet && bet > 0

  def stand : Player = Player(Wait, cards, amount)

  def hit(card : Card, playTheSplit : Boolean) : Player = cards match {
    case Left((l1, l2)) if playTheSplit => Player(status,
                                                  Left((l1, l2 :+ card)),
                                                  amount)
    case Left((l1, l2))                 => Player(status,
                                                  Left((l1 :+ card, l2)),
                                                  amount)
    case Right(l)                       => Player(status,
                                                  Right(l :+ card),
                                                  amount)
  }

  def split : Player = {
    if (canSplit) {
      val c = cards.getOrElse(List())
      return Player(status, Left(List(c.head), List(c.head)), amount)
    } else
        return this
  }

  def bet(bet : Int) : Player =
    if (canBet(bet)) {
      Player(status, cards, amount - bet)
    } else
        return this

  override def toString() : String = {
    val cardsString =
      if (cards.isLeft)
        cards.left.get._1.foldLeft("")((s, c) => s + c.toString) + " - " + cards.left.get._2.foldLeft("")((s, c) => s + c.toString)
      else
        cards.right.get.foldLeft("")((s, c) => s + c.toString)
    "P: " + status + " - Hand: " + cardsString + " - Amount: " + amount
  }
}
object Player {
  def apply(status : PlayerStatus,
            cards  : Either[(List[Card], List[Card]), List[Card]],
            amount : Int) : Player = new Player(status, cards, amount)
}

trait PlayerStatus
object Play  extends PlayerStatus {
  override def toString = "Play"
}
object Wait  extends PlayerStatus {
  override def toString = "Wait"
}
object Bust  extends PlayerStatus {
  override def toString = "Bust"
}

trait PlayerAction
object Hit      extends PlayerAction { override def toString = "Hit"       }
object HitRight extends PlayerAction { override def toString = "Hit Left"  }
object HitLeft  extends PlayerAction { override def toString = "Hit Right" }
object Stand    extends PlayerAction { override def toString = "Stand"     }
object Split    extends PlayerAction { override def toString = "Split"     }