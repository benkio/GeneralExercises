object EleventhDecember {

  sealed trait RTG { def element: String }
  case class Microchip(element: String) extends RTG
  case class Generator(element: String) extends RTG
  type Floor = Set[RTG]

  case class State(elevatorFloorNum: Int, floors: Map[Int, Floor])

  def endCondition(state: State): Boolean =
    state.elevatorFloorNum == state.floors.last._1 &&
  state.floors.init.isEmpty && !state.floors.last._2.isEmpty

  def isMicrochip(rtg: RTG): Boolean = rtg match {
    case _: Microchip => true
    case _: Generator => false
  }

  def validateFloor(floor: Floor): Boolean =
    floor
      .filter(isMicrochip(_))
      .foldLeft(true)((acc: Boolean, mc: RTG) => acc && isSafeMicrochip(mc, floor))

  def isSafeMicrochip(microchip: RTG, floor: Floor): Boolean =
    floor.filter(!isMicrochip(_)).isEmpty ||
  floor.filter(!isMicrochip(_)).exists((gen: RTG) => gen.element == microchip.element)

  def generateLoads(floor: Floor): List[Floor] =
    (floor.subsets(1) ++ floor.subsets(2)).toList.filter(validateFloor(_))

  def validateState(state: State): Boolean = state.floors.forall(x => validateFloor(x._2))

  def nextStates(state: State): List[State] =
    generateLoads(state.floors(state.elevatorFloorNum))
      .flatMap(moveLoad(state, _)).filter(validateState(_))

  def moveLoad(state:State, load: Floor): List[State] =
    List(state.elevatorFloorNum.max(0), state.elevatorFloorNum.min(state.floors.last._1))
      .map((_, load))
      .filterNot{
        case (floorNum: Int, load: Floor) =>
          floorNum == state.elevatorFloorNum ||
          (floorNum < state.elevatorFloorNum && load.size == 2)
      }
      .map{
        case (floorNum: Int, load: Floor) =>
          State(
            floorNum,
            state.floors.map {
              case (currentFloorNum: Int, load: Floor) => currentFloorNum match {
                case x if x == state.elevatorFloorNum => (x, state.floors(x).diff(load))
                case x if x == floorNum => (x, state.floors(x) ++ load)
                case x => (x, state.floors(x))
              }
            }
          )
      }

  object Input {
    def parse(s: String): State = State(
      elevatorFloorNum = 1,
      floors = Map(s.linesIterator
        .toList
        .zipWithIndex
        .map { case (line, index) => (index + 1, parseLine(line))} :_*
      ))

    def parseLine(line:String): Floor =
      line
        .split(' ')
        .drop(4)
        .filterNot(x => List("nothing", "relevant.", "a", "and").contains(x))
        .map(_.filterNot(List('.', ',').contains(_)).stripSuffix("-compatible"))
        .sliding(2, 2)
        .map(toRTG(_))
        .toSet

    def toRTG(arrayInput: Array[String]) : RTG = arrayInput(1) match {
      case "microchip" => Microchip(arrayInput(0))
      case "generator" => Generator(arrayInput(0))
    }

    val testInput: State = parse(
      s"""The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.""")

    val input : State = parse(
      scala.io.Source.fromFile("../../../input/2016/11December.txt")
        .mkString
    )


  }


  def main(args: Array[String]) : Unit = {
    println(Input.testInput)
    println(Input.input)
  }
}
