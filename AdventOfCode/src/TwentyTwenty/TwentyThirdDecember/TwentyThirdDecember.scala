object TwentyThirdDecember {
  def main(args: Array[String]) : Unit = {

    val input : Array[Int] = scala.io.Source.fromFile("../../../input/2020/23December.txt")
      .mkString
      .init
      .map(_.asDigit)
      .toArray

    println("-"*100)
    println("Real Section")
    println("-"*100)

    //Part1
    val result = CrubCups.compute(input(0), 100, CrubCups.indicesList(input))
    println(s"result: ${CrubCups.rebuildLinkedList(1,result).mkString}")
    //Part2
    val inputExtended = CrubCups.indicesListExtended(input)
    val result2 = CrubCups.compute(input(0), 10000000, inputExtended)
    val firstStar : Long = result2(1)
    val secondStar : Long = result2(firstStar.toInt)
    val ResultPart2 : Long = firstStar * secondStar
    println(s"Result part 2 - ${ResultPart2}")

    // Tester /////////////////////////////////////////////////////////////////
    println("-"*100)
    println("Test Section")
    println("-"*100)


    val inputTest : Array[Int] = Array(3,8,9,1,2,5,4,6,7)
    //Part1
    val resultTest = CrubCups.compute(inputTest(0), 100, CrubCups.indicesList(inputTest))
    println(s"resultTest: ${CrubCups.rebuildLinkedList(1,resultTest).mkString}")

    //Part2
    val inputTestExtended = CrubCups.indicesListExtended(inputTest)
    val resultTest2 = CrubCups.compute(inputTest(0), 10000000, inputTestExtended)
    val firstTestStar : Long = resultTest2(1)
    val secondTestStar : Long = resultTest2(firstTestStar.toInt)
    val testResultPart2 : Long = firstTestStar * secondTestStar
    println(s"testResult part 2 - ${testResultPart2}")
  }
}

object CrubCups {

  def compute(currentCup : Int, moves: Int, cups : Array[Int]) : Array[Int] = {
    var curCup = currentCup
    var m = 0
    for (i <- 1 to moves) {
      val a : Int = cups(curCup)
      val b : Int = cups(a)
      val c : Int = cups(b)
      var dest : Int = if (curCup - 1 > 0) curCup -1 else cups.length -1
      while (List(a, b, c).contains(dest)) {
        dest = if (dest - 1 > 0) dest -1 else cups.length -1
      }
      cups(curCup) = cups(c)
      val succDest : Int = cups(dest)
      cups(dest) = a
      cups(c) = succDest
      curCup = cups(curCup)
      m = i
    }
    cups
  }

  def rebuildLinkedList(currentCup: Int, indices: Array[Int]) : Array[Int] = indices
    .foldLeft(Array(indices(currentCup)))((acc: Array[Int], _: Int ) =>
      acc :+ indices(acc.last % indices.length)
    )

  def indicesList(indices: Array[Int]) : Array[Int] =
    indices
      .zip(indices.tail)
      .:+((indices.last, indices.head))
      .foldLeft(new Array[Int](indices.length +1)){
        case (acc : Array[Int], (i:Int, v:Int)) => {acc(i) = v ; acc}
      }

  def indicesListExtended(indices: Array[Int]) : Array[Int] =
    indices
      .zip(indices.tail)
      .:+((indices.last, indices.max + 1))
      .foldLeft(new Array[Int](indices.length +1)){
        case (acc : Array[Int], (i:Int, v:Int)) => {acc(i) = v ; acc}
      }.++((indices.max + 2) to 1000000).:+(indices(0))

}
