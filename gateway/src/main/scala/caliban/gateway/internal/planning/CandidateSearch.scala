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
        candidates(values.size).flatMap { _ =>
          var remaining    = values
          var successes    = List.empty[B]
          var firstFailure = Option.empty[PlanningFailure]
          var stopped      = Option.empty[PlanningFailure]
          while ((remaining ne Nil) && stopped.isEmpty) {
            expand match {
              case Left(failure) => stopped = Some(failure)
              case Right(_)      => ()
            }
            if (stopped.isEmpty) {
              evaluate(remaining.head) match {
                case Right(candidate)                   => successes = candidate :: successes
                case Left(failure) if failure.exhausted => stopped = Some(failure)
                case Left(failure)                      => firstFailure = firstFailure.orElse(Some(failure))
              }
              remaining = remaining.tail
            }
          }
          stopped match {
            case Some(failure)              => Left(failure)
            case None if successes.nonEmpty => Right(successes.reverse)
            case None                       => Left(firstFailure.getOrElse(PlanningFailure("No complete route candidate was found.")))
          }
        }
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
