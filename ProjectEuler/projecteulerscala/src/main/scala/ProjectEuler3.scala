import io.Source._
import scala.collection._
import scala.annotation._
import scala.math._
import ProjectEuler2._

object ProjectEuler3:

  def findAmicableNumbers(input: Int): Option[(Int, Int)] =
    val amicableCandidate = findFactors(input.toLong).sum
    val amicableCandidateFactorsSum =
      findFactors(amicableCandidate.toLong).sum
    if amicableCandidateFactorsSum == input && input != amicableCandidate then
      Some((input, amicableCandidate))
    else None

  @tailrec
  def findAmicableNumbersSeq(input: List[Int], amicables: Set[Int]): List[Int] = input match {
    case (x :: xs) if amicables(x) => findAmicableNumbersSeq(xs, amicables)
    case (x :: xs) if !amicables(x) =>
      findAmicableNumbers(x) match {
        case Some(a1, a2) => findAmicableNumbersSeq(xs, Set(a1, a2) ++ amicables)
        case None         => findAmicableNumbersSeq(xs, amicables)
      }
    case _ => amicables.toList
  }

  def es21: Int = findAmicableNumbersSeq(List.range(1, 10000), Set.empty).sum

  /////////////////////////////////////////////////////////////////////////////

  def es22Input: List[String] =
    fromInputStream(getClass.getResourceAsStream("/p022_names.txt")).mkString
      .split("\",\"")
      .map(_.replace("\"", ""))
      .sorted
      .toList

  val alphabethIndexed: List[(Char, Int)] =
    ('A' to 'Z').zip(1 to 26).toList

  def calculateNameScore(name: String): Int =
    name.map(c => alphabethIndexed.find(_._1 == c).map(_._2).getOrElse(0)).sum

  def es22: Int =
    es22Input
      .zip(LazyList.from(1))
      .map { case (name, index) =>
        calculateNameScore(name) * index
      }
      .sum

  ///////////////////////////////////////////////////////////////////////////

  val limit: Int = 28123

  def isAbundant(value: Int): Boolean =
    findFactors(value.toLong).sum > value

  lazy val abundants: List[Int] = List.range(12, limit + 1).filter(isAbundant)

  @tailrec
  def abundantsSum(acc: Set[Int], abs: List[Int]): Set[Int] = abs match {
    case Nil        => acc
    case ab :: rest => abundantsSum(acc ++ Set.from(abs.map(_ + ab).filter(_ <= limit)), rest)
  }

  lazy val abundantsSums: Set[Int] = abundantsSum(Set.empty, abundants)

  def es23: Int =
    List.range(1, limit + 1).foldLeft(0)((acc, x) => if abundantsSums(x) then acc else acc + x)
