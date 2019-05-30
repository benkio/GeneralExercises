package com.analytics.pipes

import fs2.Pipe
import java.util.UUID
import java.time._
import com.analytics.model.core._
import cats.effect._

/**
  * VisitAggregation
  * Aggregate the visits into VisitSummary
  *
  */
object VisitAggregation {

  /**
    * filterAggregatedVisit - Remove from the aggregated map the old visits
    * (default older then 1h)
    */
  def filterAggregatedVisit(aggregatedVisit: VisitAggregation,
                            notOlderThen: Duration,
                            currentTime: LocalDateTime): VisitAggregation ={
    val visitAggregatedNewestVisitTime = aggregatedVisit.values.map(_.newestUpdate).toList.sorted.lastOption
    aggregatedVisit.filterNot {
      case (_: UUID, vs: VisitSummary) => {
        val visitSummaryTime = vs.lastUpdate.getOrElse(vs.createdAt)
        val newestVisitTime =
          visitAggregatedNewestVisitTime.fold(visitSummaryTime)(
            (vt : LocalDateTime) =>
            if (visitSummaryTime.compareTo(vt) < 0) vt else visitSummaryTime
          )
        visitSummaryTime.isBefore(newestVisitTime.minus(notOlderThen))
      }
    }
  }

  /**
    * aggregateVisitPipe - Catches the errors and filter older values
    */
  def aggregateVisitPipe[F[_]](validVisitWindow : Duration): Pipe[F, Visit, (VisitAggregation, List[Visit])] =
    in => in.scan[(VisitAggregation, List[Visit])]((Map.empty[UUID, VisitSummary],
                                                    List.empty[Visit]))(
      (acc: (VisitAggregation, List[Visit]), m: Visit) => {
        aggregateVisit(acc._1, m)
          .map((newVisitAggregation : VisitAggregation) =>
            filterAggregatedVisit(newVisitAggregation,
                                  validVisitWindow,
                                  m.time))
          .fold((acc._1, m :: acc._2))((newVisitAggregation: VisitAggregation) => (newVisitAggregation, acc._2))
      })

  /**
    * aggregateVisit - returns None if some misordering happens otherwise update
    * the accumulator data.
    */
  def aggregateVisit(accumulator: VisitAggregation,
                     message: Visit): Option[VisitAggregation] = (accumulator.get(message.id), message) match {
    case (Some(currentValue), y: VisitUpdate) =>
      Some(accumulator.updated(y.id,
                               VisitSummary.applyUpdate(y, currentValue)))
    case (None, y: VisitCreate) =>
      Some(accumulator + ((y.id, VisitSummary(y))))
    case _ => None
  }

  /**
    * handleAggregationErrors - catches errors from the stream and it handle them.
    */
  def handleAggregationErrors: Pipe[IO, (VisitAggregation, List[Visit]), VisitAggregation] =
    in => in.evalMap[IO, VisitAggregation](
      (acc: (VisitAggregation, List[Visit])) => for {
        //commented out this line, too noisy. Here is where the ordering error
        //handling happens.
        _ <- IO(()// acc._2.foreach((errorVisit: Visit) => println(s"error trying to aggregate the visit $errorVisit"))
        )
      } yield acc._1
    )

  /**
    * aggregateVisit - Pipe composition
    */
  def aggregateVisit(validVisitWindow : Duration): Pipe[IO, Visit, VisitAggregation] =
    aggregateVisitPipe(validVisitWindow) andThen handleAggregationErrors
}
