package IO

import scala.io.StdIn._
import scala.util.Try
import domain._

object EntryPoint {
  def main(args : Array[String]) : Unit = {

    def retryInt : Int = Try(readInt()).getOrElse({
                                                    println("the input is not a number, please reinsert")
                                                    retryInt
                                                  })
    println("--------------TWENTYONE GAME----------------")
    println("Insert the number of Players: ")
    val numberOfPlayers : Int = retryInt
    Game.run(numberOfPlayers)
  }
}

object User {
  def askForAction(p : Player) : PlayerAction = {
    println("turn of the player: " + p)
    (p.status, p.cards) match {
      case (Play, Right(_)) => {
        println("choose your action(1 - Hit, 2 - Stand, 3 - Split): ")
        Try(readInt()).getOrElse({
                                   println("The input is not a number, retry")
                                   askForAction(p)
                                 }) match {
          case 1 => Hit
          case 2 => Stand
          case 3 => if (p.canSplit) Split else {
            println("player cannot split")
            askForAction(p)
          }
          case _ => {
            println("action not recognized, please reinsert")
            askForAction(p)
          }
        }
      }
      case (Play, Left((_, _))) => {
        println("choose your action(1 - Hit Left, 2 - Hit Right, 3 - Stand): ")
        Try(readInt()).getOrElse({
                                   println("The input is not a number, retry")
                                   askForAction(p)
                                 }) match {
          case 1 => HitLeft
          case 2 => HitRight
          case 3 => Stand
          case _ => {
            println("action not recognized, please reinsert")
            askForAction(p)
          }
        }
      }
      case _ => throw new Exception("Player cannot perform any action, is bust or has already played")
    }
  }

  def printPlayerActionResult(pa : PlayerAction,
                              points : Either[(Int,Int),Int],
                              p : Player) {
    println("Player action " + pa + " result")
    println("Player: " + p)
    println("CurrentPoints: " + (if (points.isRight) points.right.get else points.left.get))
  }

  def askForBet(p : Player) : Int = {
    println("Bet turn of player "+ p)
    print("insert your bet(amount " + p.amount  +"): ")
    Try(readInt()).getOrElse({
                               println("the input is not a number, retry")
                               askForBet(p)
                             })
  }

  def printBankStatus(bank : Player,
                      points : Either[(Int,Int),Int]) = {
    println("bank: " + bank)
    println("bank points: " + (if (points.isRight) points.right.get else points.left.get))
  }

  def printWinAmount(winAmount : Int) = {
    println("winAmount: " + winAmount)
  }

  def printPlayerStatus(lp : List[Player]) = {
    lp.foreach(p => {
                 println("Player: " + p)
               })
  }

  def askForNewGame() : Boolean = {
    println("Do you want to play a new game?(1 - Yes, 2 - No)")
    Try(readInt()).getOrElse({
                              println("The input is not a number, retry")
                              askForNewGame()
                            }) == 1
  }
}