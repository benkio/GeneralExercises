import scala.math._
import scala.annotation.tailrec

object ProjectEuler:
  def es1: Int =
    (1 until 1000)
      .filter(x => x % 3 == 0 || x % 5 == 0)
      .sum

  def es2: Int =
    def go(acc: Int, prev: Int, next: Int): Int = (prev + next) match {
      case x if x > 4000000 => acc
      case x if x % 2 == 0 => go(acc + x, next, x)
      case x => go(acc, next, x)
    }
    go(0, 0, 1)

  def es3: Long =
    val input: Long = 600851475143L
    def largestPrimeFactor(x: Long): Long =
      def go(factor: Int, target: Long): List[Long] = (factor, target) match {
        case (_, t) if MathOps.isPrime(t) => List(1, t)
        case (f, t) if MathOps.isPrime(f) && t % f == 0 => f :: go(2, target / f)
        case _ => go(factor + 1, target)
      }
      go(2, x).max
    largestPrimeFactor(input)

  def es4: Int =
    val threeDigitNums = (101 to 999).reverse
    threeDigitNums.flatMap(d => threeDigitNums.map(d * _)).filter(MathOps.isPalindrome).max

  def es5: Int =
    LazyList.from(20, 20).find(x => (1 to 20).reverse.forall(x % _ == 0)).get

  def es6: Int =
    val sumOfSquare = (1 to 100).map(pow(_, 2).toInt).sum
    val squareOfSum = pow((1 to 100).sum, 2).toInt
    squareOfSum - sumOfSquare

  def es7: Int =
    LazyList
      .from(2, 1)
      .filter(MathOps.isPrime)
      .take(10001)
      .last

  def es8: Long =
    val input =
      "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    input.sliding(13, 1).map(_.map(_.asDigit.toLong).fold(1L)(_ * _)).max

  def es9: Int = (for {
    a <- LazyList.range(1, 999, 1)
    b <- LazyList.range(1, 1000 - a, 1)
    c = 1000 - (a + b)
    if (
      (pow(a, 2) + pow(b, 2) == pow(c, 2)) ||
        (pow(b, 2) + pow(c, 2) == pow(a, 2)) ||
        (pow(c, 2) + pow(a, 2) == pow(b, 2))
    )
  } yield a * b * c).head

  def es10: Long =
    @tailrec
    def go(curr: Long, acc: Long): Long = curr match {
      case x if x >= 2000000       => acc
      case x if MathOps.isPrime(x) => go(x + 1, acc + x)
      case x                       => go(x + 1, acc)
    }
    go(2, 0)
