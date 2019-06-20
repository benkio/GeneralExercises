package setgame.inputgenerator

import org.scalacheck.Gen

object InputGenerator extends CardGenerator {

  def generateDistinct[T](gen : Gen[T], n : Int) : Gen[List[T]] =
    Gen.containerOfN[Set, T](n, gen).map(_.toList) suchThat { (l : List[T]) => l.size == n }

}
