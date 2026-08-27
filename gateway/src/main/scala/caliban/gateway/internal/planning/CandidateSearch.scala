package caliban.gateway.internal.planning

import caliban.gateway.internal.planning.CandidateSearch._
import zio.Duration

/**
 * A planning-session budget shared by recursive candidate exploration.
 */
private[gateway] final class CandidateSearch(limits: Limits) {
  private val startedAt  = System.nanoTime()
  private var considered = 0L
  private var expanded   = 0

  def check: Either[PlanningFailure, Unit] =
    if (System.nanoTime() - startedAt >= limits.timeout.toNanos)
      exhausted("Route planning exceeded the configured duration limit.")
    else Right(())

  def combine[A, B, C](left: List[A], right: List[B])(combine: (A, B) => C): Either[PlanningFailure, List[C]] =
    if (left.isEmpty || right.isEmpty) check.map(_ => Nil)
    else if (left.tail.isEmpty && right.tail.isEmpty) check.map(_ => combine(left.head, right.head) :: Nil)
    else {
      val count = left.size.toLong * right.size.toLong
      capacity(count).map(_ => left.flatMap(a => right.map(combine(a, _))))
    }

  def evaluate[A, B](values: List[A])(
    evaluate: A => Either[PlanningFailure, B]
  ): Either[PlanningFailure, List[B]] =
    values match {
      case value :: Nil => check.flatMap(_ => evaluate(value)).map(List(_))
      case Nil          => check.flatMap(_ => Left(PlanningFailure("No complete route candidate was found.")))
      case _            =>
        for {
          _      <- candidates(values.size)
          result <- values
                      .foldLeft[Either[PlanningFailure, (List[B], Option[PlanningFailure])]](Right(Nil -> None)) {
                        case (state, value) =>
                          state.flatMap { case (successes, firstFailure) =>
                            expand.flatMap { _ =>
                              // Invalid alternatives can be skipped, but exhausting the shared budget stops the search.
                              evaluate(value) match {
                                case Right(candidate)                   =>
                                  Right((candidate :: successes) -> firstFailure)
                                case Left(failure) if failure.exhausted => Left(failure)
                                case Left(failure)                      =>
                                  Right(successes -> firstFailure.orElse(Some(failure)))
                              }
                            }
                          }
                      }
                      .flatMap { case (successes, firstFailure) =>
                        if (successes.nonEmpty) Right(successes.reverse)
                        else Left(firstFailure.getOrElse(PlanningFailure("No complete route candidate was found.")))
                      }
        } yield result
    }

  private def capacity(count: Long): Either[PlanningFailure, Unit] =
    if (count > limits.maxCandidates.toLong - considered)
      exhausted("Route planning exceeded the configured candidate limit.")
    else check

  private def candidates(count: Long): Either[PlanningFailure, Unit] =
    if (count <= 1) check
    else
      capacity(count).flatMap { _ =>
        considered += count
        check
      }

  private def expand: Either[PlanningFailure, Unit] =
    if (expanded >= limits.maxExpansions)
      exhausted("Route planning exceeded the configured expansion limit.")
    else {
      expanded += 1
      check
    }

  private def exhausted(message: String): Either[PlanningFailure, Unit] =
    Left(PlanningFailure(message, exhausted = true))
}

private[gateway] object CandidateSearch {
  final case class PlanningFailure(message: String, exhausted: Boolean = false)

  final case class Limits(maxCandidates: Int, maxExpansions: Int, timeout: Duration)

}
