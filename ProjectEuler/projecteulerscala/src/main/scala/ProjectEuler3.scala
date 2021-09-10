import scala.collection._
import scala.annotation._
import scala.math._
import ProjectEuler2._

object ProjectEuler3:

  def findAmicableNumbers(input: Int): Option[(Int, Int)] =
    val amicableCandidate = findFactors(input.toLong).filterNot(_ == input).sum
    val amicableCandidateFactorsSum =
      findFactors(amicableCandidate.toLong).filterNot(_ == amicableCandidate).sum
    if amicableCandidateFactorsSum == input && input != amicableCandidate then
      Some((input, amicableCandidate))
    else None

  def findAmicableNumbersSeq(input: List[Int], amicables: Set[Int]): List[Int] = input match {
    case (x :: xs) if amicables(x) => findAmicableNumbersSeq(xs, amicables)
    case (x :: xs) if !amicables(x) =>
      findAmicableNumbers(x).fold(
        findAmicableNumbersSeq(xs, amicables)
      ) { case (a1, a2) => findAmicableNumbersSeq(xs, Set(a1, a2) ++ amicables) }
    case _ => amicables.toList
  }

  def es21: Int = findAmicableNumbersSeq(List.range(1, 10000), Set.empty).sum
