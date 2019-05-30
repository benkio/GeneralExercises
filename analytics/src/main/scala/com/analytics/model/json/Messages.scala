package com.analytics.model.json

import com.analytics.model.core._

/**
  * Message
  * Trait grouping the json representation of a json message
  *
  */
sealed trait Message {
  def visit: Visit
}

case class JVisitCreate private(messageType: String, visit: VisitCreate) extends Message

case class JVisitUpdate private(messageType: String, visit: VisitUpdate) extends Message

// Smart Constructors. Fix the messageType ////////////////////////////////////

object JVisitCreate {
  def apply(visit: VisitCreate): JVisitCreate = new JVisitCreate("VisitCreate", visit)
}

object JVisitUpdate {
  def apply(visit: VisitUpdate): JVisitUpdate = new JVisitUpdate("VisitUpdate", visit)
}