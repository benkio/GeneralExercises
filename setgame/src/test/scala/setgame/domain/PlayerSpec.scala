package setgame.domain

import setgame.domain._
import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import setgame.inputgenerator._
import cats.effect.IO
import cats.data._
import java.util.UUID

object PlayerScalaCheckSpec extends Properties("Player") {

  def gameStateTTestPattern[T](
    deck : Deck,
    players : Set[Player],
    stateTToTest : StateT[IO, GameState, T],
    testFunction : (GameState, GameState, T) => Boolean) : Boolean = {
        val inputGamestate : IO[GameState] =
          GameBoard.build.runA(deck).map((gb : GameBoard) =>
            GameState(
              players,
              deck,
              gb
            )
          )
        inputGamestate.flatMap((gs : GameState) =>
          stateTToTest
            .run(gs)
            .map{ case (resultGs, resultSet) =>
              testFunction(gs, resultGs, resultSet)
            }
        ).unsafeRunSync == true
  }

  property("apply returns always players with 0 score and unique UUID") =
    forAll(Gen.posNum[Int]){
      (n : Int) => {
        val players = List.fill(n)(Player.apply)
        players.forall(_.score == 0) && players.map(_.id).distinct.size == n
      }
  }

  property("dumbPlayerStrategy return the same gamestate + None if the first 3 cards of the board are not a set") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (inputCards : List[Card]) => {
        val deck : Deck = Deck(List(inputCards(0),
          inputCards(0),
          inputCards(1)) ++ List.fill(20)(inputCards(0)))
        gameStateTTestPattern[Option[GameSet]](
          deck,
          Set.empty[Player],
          Player.dumbPlayerStrategy,
          (gs, resultGs, resultSet) => resultGs == gs &&
              resultSet.isEmpty == true
        )
      }
    }

  property("move should return the same input gamestate if the player guess wrong") =
        forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (inputCards : List[Card]) => {
        val deck : Deck = Deck(List(inputCards(0),
          inputCards(0),
          inputCards(1)) ++ List.fill(20)(inputCards(0)))
        val player = Player.apply
        gameStateTTestPattern[Unit](
          deck,
          Set(player),
          Player.move(player),
          (gs, resultGs, _) => resultGs == gs
        )
      }
    }

}

class PlayerSpec extends WordSpec with Matchers {

  import setgame.inputgenerator.FixedInput._

  val validDecksInput : Iterator[Deck] =
    validDifferentCardInput
      .permutations
      .take(20)
      .map((lt : List[(Card, Card, Card)]) =>
        Deck(
          lt.flatMap { case (c1, c2, c3) => List(c1, c2, c3) }
        )
      )

  "Player - dumbPlayerStrategy" should {
    "return Some if the first 3 cards of the board are a set" in {
      validDecksInput
        .forall((d : Deck) =>
          PlayerScalaCheckSpec.gameStateTTestPattern[Option[GameSet]](
            d,
            Set.empty[Player],
            Player.dumbPlayerStrategy,
            (inputGs, resultGs, resultSet) =>{
              resultSet.isDefined &&
              resultGs.board.cards.size == 12 &&
              inputGs.board != resultGs.board
            }
          )
        )
    }
  }

  "Player - Move" should {
    "return a gamestate with the input player having the current score + 1 if the first 3 cards of the board are a set" in {
      validDecksInput
        .forall((d : Deck) => {
          val player = Player.apply
          PlayerScalaCheckSpec.gameStateTTestPattern[Unit](
            d,
            Set(player),
            Player.move(player),
            (_, resultGs, _) =>{
              resultGs.players.size == 1 &&
              resultGs.players.head.score == 1
            }
          )}
        )
    }
  }
}
