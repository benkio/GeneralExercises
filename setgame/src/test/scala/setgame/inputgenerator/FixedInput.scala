package setgame.inputgenerator

import setgame.domain._

object FixedInput {

  val validDifferentCardInput : List[(Card, Card, Card)] =
    List(
     (	Card(Oval	, Red	,Two	,Solid),
	Card(Oval	, Purple,One	,Stripe),
	Card(Oval	, Green	,Three	,Outline)),
     (	Card(Squiggle	, Red	,One	,Outline),
	Card(Diamond	, Green	,Two	,Outline),
	Card(Oval	, Purple,Three	,Outline)),
     (	Card(Squiggle	, Red	,Two	,Outline),
	Card(Oval	, Green	,One	,Outline),
	Card(Diamond	, Purple,Three	,Outline)),
     (	Card(Diamond	, Purple,Three	,Solid),
	Card(Oval	, Purple,Two	,Stripe),
	Card(Squiggle	, Purple,One	,Outline)),
     (	Card(Squiggle	, Red	,Two	,Solid),
	Card(Squiggle	, Green	,Three	,Solid),
	Card(Squiggle	, Purple,One	,Solid)),
     (	Card(Diamond	, Red	,Two	,Solid),
	Card(Diamond	, Red	,One	,Stripe),
	Card(Diamond	, Red	,Three	,Outline)))
}
