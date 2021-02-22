object EleventhDecember {

  sealed trait RTG { def element: String }
  case class Microchip(element: String) extends RTG
  case class Generator(element: String) extends RTG
  type Floor = Set[RTG]

  case class State(elevatorFloorNum: Int, floors: Map[Int, Floor])

  def endCondition(state: State): Boolean =
    state.elevatorFloorNum == state.floors.last._1 &&
  state.floors.init.values.foldLeft(Set.empty[RTG])(_ ++ _).isEmpty &&
  !state.floors.last._2.isEmpty

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

  def generateLoads(floor: Floor): Set[Floor] =
    (floor.subsets(1) ++ floor.subsets(2)).filter(validateFloor(_)).toSet

  def validateState(state: State): Boolean = state.floors.forall(x => validateFloor(x._2))

  def nextStates(state: State): Set[State] =
    generateLoads(state.floors(state.elevatorFloorNum))
      .flatMap(moveLoad(state, _)).filter(validateState(_))

  def moveLoad(state:State, load: Floor): Set[State] = {
    def toState(loads: List[(Int, Floor)]): Set[State] =
      loads.map{
        case (floorNum: Int, load: Floor) =>
          State(
            floorNum,
            state.floors.map {
              case (currentFloorNum: Int, oldFloor: Floor) => currentFloorNum match {
                case x if x == state.elevatorFloorNum => (x, oldFloor.diff(load))
                case x if x == floorNum => (x, oldFloor ++ load)
                case x => (x, oldFloor)
              }
            }
          )
      }.toSet

    val (uploads, downloads) = List(
      (state.elevatorFloorNum - 1).max(1),
      (state.elevatorFloorNum + 1).min(state.floors.last._1)
    )
      .map((_, load))
      .filterNot{
        case (floorNum: Int, load: Floor) =>
          floorNum == state.elevatorFloorNum ||
          (floorNum < state.elevatorFloorNum && load.size == 2)
      }.partition(x =>
        x._1 > state.elevatorFloorNum && x._2.size == 2
      )

    if (uploads.isEmpty) toState(downloads) else toState(uploads)
  }

  import collection.parallel.ParSet

  def solution(states: ParSet[State], chain: ParSet[State], step: Int): Option[Int] = {
    val validStates = states.filterNot(x => chain.exists(y => stateEquality(x, y)))
    validStates match {
      case xs if xs.exists(endCondition(_)) => Some(step)
      case xs if xs.isEmpty => None
      case xs => {
        val ns = xs.par.flatMap(nextStates(_))
        solution(
          ns.tail.foldLeft(ParSet(ns.head))((acc: ParSet[State], st: State) => if (acc.exists(st1 => stateEquality(st1,st))) acc else acc + st),
          chain ++ states,
          step + 1
        )
      }
    }
  }

  def stateEquality(state1: State, state2: State) : Boolean =
    state1.elevatorFloorNum == state2.elevatorFloorNum &&
  state1.floors.values.zip(state2.floors.values)
    .forall{
      case (s1,s2) =>
        s1.count(isMicrochip(_)) == s2.count(isMicrochip(_)) &&
        s1.count(!isMicrochip(_)) == s2.count(!isMicrochip(_))
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
    val sol2InitialState: State = State(
      Input.input.elevatorFloorNum,
      Input.input.floors.map { case (fln, fl) => if (fln == 1) (fln, fl ++ Set(Generator("elerium"), Generator("dilithium"), Microchip("elerium"), Microchip("dilithium"))) else (fln, fl) }
    )
    println("inputTest: " + solution(ParSet(Input.testInput), ParSet.empty, 0).get)
    println("solution1: " + solution(ParSet(Input.input), ParSet.empty, 0).get)
    println("solution2: " + solution(ParSet(sol2InitialState), ParSet.empty, 0).get)
  }
}
