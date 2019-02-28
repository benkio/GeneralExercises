package domain

object GameConstraints {
  val bustThreshold : Int = 21
  val startingAmount : Int = 100
}

object PointCalculator {
  def calculatePoints(cards : List[Card]) : Int = {
    cards.map(c => calculatePoint(c)).fold(0)(_ + _)
  }

  def calculatePoint(card : Card) : Int = card match {
    case King()       => 3
    case Queen()      => 2
    case Jack()       => 1
    case Ace()        => 11
    case valueCard(v) => v
  }

  def checkPlayerPoints(p : Player) : (Either[(Int,Int),Int], Player) =
    p.cards match {
      case Left((l1, l2)) => (calculatePoints(l1), calculatePoints(l2)) match {
        case (x, y) if x > GameConstraints.bustThreshold && y > GameConstraints.bustThreshold => (Left((x, y)), Player(Bust, p.cards, p.amount))
        case (x, y) if x > GameConstraints.bustThreshold && y <= GameConstraints.bustThreshold => (Right(y), Player(p.status, Right(l2), p.amount))
        case (x, y) if x <= GameConstraints.bustThreshold && y > GameConstraints.bustThreshold => (Right(x), Player(p.status, Right(l1), p.amount))
        case (x, y) if x <= GameConstraints.bustThreshold && y <= GameConstraints.bustThreshold => (Left((x, y)), Player(p.status, p.cards, p.amount))
      }
      case Right(l)       => calculatePoints(l) match {
        case x if x > GameConstraints.bustThreshold => (Right(x), Player(Bust, p.cards, p.amount))
        case x => (Right(x), Player(p.status, p.cards, p.amount))
      }
    }
}

object Game {

  type Bet = Int

  def run(numberOfPlayers : Int) : Unit = {

    def innerGame(deck : Deck, players : List[Player]) : Unit = {
      val (deck2, players2) = distributeOneCardToEveryone(deck, players)
      val (deck3, betPlayer) = players2.map(p => betTurn(p))
        .foldLeft(deck2, List() : List[(Bet, Player)])(
          (state, lbp) => {
            val (newDeck, newPalyer) = playerActionRecursion(state._1, lbp._2)
            (newDeck, state._2 :+ (lbp._1, newPalyer))
          }
        )
      val (deck4, bank) = Bank.play(deck3)
      IO.User.printBankStatus(bank,
                              PointCalculator.checkPlayerPoints(bank)._1)
      val totalBet = betPlayer.map(_._1).foldLeft(0)(_+_)
      val players3 = betPlayer.map(_._2)
      val winAmount = if (players3.exists(p => winCordition(bank, p)))
                        (totalBet / players3.count(p => winCordition(bank, p))).toInt
                      else totalBet
      val players4 = players3.map(
        p => {
          if (winCordition(bank, p)) Player(Wait, Right(List()), p.amount + winAmount)
          else Player(Wait, Right(List()), p.amount)
        }
      )

      IO.User.printWinAmount(winAmount)
      IO.User.printPlayerStatus(players3)
      IO.User.printPlayerStatus(players4)
      val newGame : Boolean = IO.User.askForNewGame()
      if (newGame) innerGame(deck4, players4)
    }

    val deck = Deck.createDeck(numberOfPlayers)
    val startingPlayers = setupPlayers(numberOfPlayers)
    innerGame(deck, startingPlayers)
  }

  def distributeOneCardToEveryone(deck : Deck, lps : List[Player]) : (Deck, List[Player]) = {
    lps.foldLeft((deck, List() : List[Player]))(
      (state, p) => {
        val (card, newDeck) = Deck.getCard(state._1)
        (newDeck, state._2 :+ Player(Play, Right(List(card)), p.amount))
      }
    )
  }

  def setupPlayers(numberOfPlayers : Int) : List[Player] = {
    List.fill(numberOfPlayers)(Player(Wait,
                                      Right(List()),
                                      GameConstraints.startingAmount))
  }

  def winCordition(bank : Player, p: Player) : Boolean = {
    val bankPoints : Int = PointCalculator.checkPlayerPoints(bank)._1.right.get
    p.status != Bust &&
      ( bank.status == Bust ||
         PointCalculator.checkPlayerPoints(p)._1
         .fold[Boolean]((p : (Int, Int)) => p._1 > bankPoints || p._2 > bankPoints,
                        (p : Int) => p > bankPoints))
  }

  def betTurn(p : Player) : (Bet, Player) = {
    val bet = IO.User.askForBet(p)
    if (p.canBet(bet)) (bet, p.bet(bet))
    else betTurn(p)
  }

  def playerActionRecursion(d : Deck, p : Player) : (Deck, Player) = p match {
    case Player(Play, Right(c), a) => {
      val nextAction : PlayerAction = IO.User.askForAction(p)
      nextAction match {
        case Hit => {
          val (card, newDeck) = Deck.getCard(d)
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.hit(card, true))
          IO.User.printPlayerActionResult(Hit, points, newPalyer)
          playerActionRecursion(newDeck, newPalyer)
        }
        case Stand => {
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.stand)
          IO.User.printPlayerActionResult(Stand, points, newPalyer)
          playerActionRecursion(d, newPalyer)
        }
        case Split => {
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.split)
          IO.User.printPlayerActionResult(Stand, points, newPalyer)
          playerActionRecursion(d, newPalyer)
        }
      }
    }
    case Player(Play, Left((split1, split2)), a) => {
      val nextAction : PlayerAction = IO.User.askForAction(p)
      nextAction match {
        case HitLeft => {
          val (card, newDeck) = Deck.getCard(d)
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.hit(card, false))
          IO.User.printPlayerActionResult(Hit, points, newPalyer)
          playerActionRecursion(newDeck, newPalyer)
        }
        case HitRight => {
          val (card, newDeck) = Deck.getCard(d)
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.hit(card, true))
          IO.User.printPlayerActionResult(Hit, points, newPalyer)
          playerActionRecursion(newDeck, newPalyer)
        }
        case Stand => {
          val (points, newPalyer) = PointCalculator.checkPlayerPoints(p.stand)
          IO.User.printPlayerActionResult(Stand, points, newPalyer)
          playerActionRecursion(d, newPalyer)
        }
        case _ => throw new Exception("player already splitted the hand")
      }
    }
    case _ => (d, p)
  }

  def playerEndPlay(player: Player) : Boolean =
    player.status match {
      case Wait => true
      case Play => false
      case Bust => true
    }
}

trait AIPlayer {
  def play(deck : Deck) : (Deck, Player)
}

object Bank extends AIPlayer {

  // private var player = Player(Play, Right(List()), Int.MaxValue)
  private val hitThreshold = 16

  def performAction(deck : Deck, player : Player) : (Deck, Player) = PointCalculator.checkPlayerPoints(player) match {
    case (Right(x), p) if x <= hitThreshold =>
      if (p.cards.isRight ) {
        val (card, newDeck) = Deck.getCard(deck)
        (newDeck, PointCalculator.checkPlayerPoints(p.hit(card, true))._2)
      }
      else throw new Exception("BANK CANNOT SPLIT")
    case (Right(x), p) => (deck, p.stand)
    case _ => throw new Exception("BANK CANNOT SPLIT")
  }

  def play(deck : Deck) : (Deck, Player) = {
    play(deck, Player(Play, Right(List()), Int.MaxValue))
  }

  def play(deck : Deck, player : Player) : (Deck, Player) = {
    val (newDeck : Deck, newPlayer : Player) = performAction(deck, player)
    if (Game.playerEndPlay(newPlayer))
      (newDeck, newPlayer)
    else
      play(newDeck, newPlayer)
  }
}