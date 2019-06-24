package setgame

import cats.effect._
import setgame.domain._
import cats._, cats.data._, cats.implicits._
import scala.util.Random

object Main extends IOApp {

  def run(args: List[String]): cats.effect.IO[cats.effect.ExitCode] = for {
    _ <- IO(println("Start Set game"))
    firstGamestate <- initialGameState
    _ <- IO(println(s"initialGamestate ${firstGamestate.board}"))
    finalGamestate <- gameLoop(10000).run(firstGamestate)
    _ <- IO(println(s"finalGamestate ${finalGamestate.board}"))
    winner <- IO.fromTry(GameState.winner(finalGamestate))
    _ <- IO(println(s"Players: ${finalGamestate.players} The winner is $winner!!!!!\n end of the game"))
  } yield ExitCode.Success

  def gameLoop(n : Int) : Kleisli[IO, GameState, GameState] =
    List.fill(n)(Kleisli(gameStep _))
      .foldLeft(Kleisli(IO.pure[GameState] _))(_ andThen _)


  def gameStep(gamestate : GameState): IO[GameState] = for {
    players <- IO(Random.shuffle(gamestate.players))
    playerMovedGs <- players
      .map((p : Player) => Player.move(p))
      .foldLeft(StateT.pure[IO, GameState, Unit]())(_ *> _)
    .runS(gamestate)
    // Need the next steps because of players dumb strategy. We create a new board everytime
    deckNnewBoard <- GameBoard.build.run(gamestate.deck)
    result = playerMovedGs.copy(deck = deckNnewBoard._1, board = deckNnewBoard._2)
  } yield result

  val initialGameState : IO[GameState] = for {
    newDeck <- Deck()
    players = List.fill(2)(Player.apply).toSet
    deckNgameBoard <- GameBoard.build.run(newDeck)
  } yield GameState(players, deckNgameBoard._1, deckNgameBoard._2)
}
