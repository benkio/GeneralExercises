import scala.math._

object MathOps:
  def isPrime(x: Long): Boolean =
    def go(cur: Long): Boolean = cur match {
      case y if y > (sqrt(x.toDouble).round) => true
      case y if x % y == 0L => false
      case _ => go(cur + 1)
    }
    go(2L)

  def isPalindrome(x: Int): Boolean = x.toString == x.toString.reverse
