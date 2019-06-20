package setgame.rulecheckers

import setgame.domain._

trait Checker[T] {
  def check(elem1: T, elem2 : T, elem3: T) : Boolean
}

trait FeatureChecker[T <: CardFeature] extends Checker[T] {

  def check(feature1: T, feature2 : T, feature3: T) : Boolean = {
    val (equalityList, disequalityList) =
      List(feature1, feature2, feature3)
        .combinations(2)
        .map { case Seq(x, y) => (x == y, x != y) }
        .toList
        .unzip
    equalityList.forall(_ == true) || disequalityList.forall(_ == true)
  }

}

object ShapeChecker extends FeatureChecker[Shape]
object ColorChecker extends FeatureChecker[Color]
object NumberChecker extends FeatureChecker[Number]
object ShadingChecker extends FeatureChecker[Shading]
