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

  object ParseInput {
    def parse(s: String): State = ???

  }
}
