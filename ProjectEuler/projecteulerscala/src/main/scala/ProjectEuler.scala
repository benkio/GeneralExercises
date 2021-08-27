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
