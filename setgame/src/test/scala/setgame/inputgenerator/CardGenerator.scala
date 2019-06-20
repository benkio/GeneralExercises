package setgame.inputgenerator

import org.scalacheck.Gen
import setgame.domain._

trait CardGenerator {

  val shapeGenerator : Gen[Shape] = Gen.oneOf(List(Oval, Squiggle, Diamond))
  val colorGenerator : Gen[Color] = Gen.oneOf(List(Red, Purple, Green))
  val numberGenerator : Gen[Number] = Gen.oneOf(List(One, Two, Three))
  val shadingGenerator : Gen[Shading] = Gen.oneOf(List(Solid, Stripe, Outline))

  val cardGenerator : Gen[Card] = for {
    shape <- shapeGenerator
    color <- colorGenerator
    number <- numberGenerator
    shading <- shadingGenerator
  } yield Card(shape, color, number, shading)
}
