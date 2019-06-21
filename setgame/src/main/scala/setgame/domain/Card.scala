package setgame.domain

sealed trait CardFeature

sealed trait Shape extends CardFeature
case object Oval extends Shape
case object Squiggle extends Shape
case object Diamond extends Shape

object Shape {
  val values : List[Shape] = List(Oval, Squiggle, Diamond)
}

sealed trait Color extends CardFeature
case object Red extends Color
case object Purple extends Color
case object Green extends Color

object Color {
  val values : List[Color] = List(Red, Purple, Green)
}

sealed trait Number extends CardFeature
case object One extends Number
case object Two extends Number
case object Three extends Number

object Number {
  val values : List[Number] = List(One, Two, Three)
}

sealed trait Shading extends CardFeature
case object Solid extends Shading
case object Stripe extends Shading
case object Outline extends Shading

object Shading {
  val values : List[Shading] = List(Solid, Stripe, Outline)
}

case class Card(shape: Shape, color: Color, number: Number, shading: Shading)
