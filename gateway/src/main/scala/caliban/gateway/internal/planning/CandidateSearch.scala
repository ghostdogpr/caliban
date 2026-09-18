package caliban.gateway.internal.planning

import caliban.gateway.internal.planning.CandidateSearch._
import zio.Duration

import scala.annotation.tailrec

/**
 * A planning-session budget shared by recursive candidate exploration.
 */
private[gateway] final class CandidateSearch(limits: Limits) {
  private val startedAt      = System.nanoTime()
  private var candidateCount = 0L
  private var expansionCount = 0

  def checkTimeout: Either[PlanningFailure, Unit] =
    if (System.nanoTime() - startedAt >= limits.timeout.toNanos)
      exhausted("Route planning exceeded the configured duration limit.")
    else Right(())

  def combine[A, B, C](left: List[A], right: List[B])(combine: (A, B) => C): Either[PlanningFailure, List[C]] =
    (left, right) match {
      case (Nil, _) | (_, Nil)  => checkTimeout.map(_ => Nil)
      case (a :: Nil, b :: Nil) => checkTimeout.map(_ => combine(a, b) :: Nil)
      case _                    =>
        val count = left.size.toLong * right.size.toLong
        checkCapacity(count).map(_ => left.flatMap(a => right.map(combine(a, _))))
    }

  /**
   * Accumulates inputs without introducing search branches or charging the candidate budget.
   */
  def fold[A, B](inputs: List[A], initial: B)(step: (B, A) => Either[PlanningFailure, B]): Either[PlanningFailure, B] =
    inputs.foldLeft[Either[PlanningFailure, B]](Right(initial)) { (result, input) =>
      result.flatMap(step(_, input))
    }

  /**
   * Expands dependent alternatives with the same rejection and budget rules as evaluate.
   */
  def expandAll[A, B](inputs: List[A], initial: B)(
    step: (B, A) => Either[PlanningFailure, List[B]]
  ): Either[PlanningFailure, List[B]] =
    fold(inputs, List(initial))((states, input) => flatEvaluate(states)(step(_, input)))

  def flatEvaluate[A, B](values: List[A])(
    evaluate: A => Either[PlanningFailure, List[B]]
  ): Either[PlanningFailure, List[B]] =
    this.evaluate(values)(evaluate).map(_.flatten)

  /**
   * Rejected routes are skipped; exhausting a budget stops the entire search. A single route does not branch.
   */
  def evaluate[A, B](values: List[A])(evaluate: A => Either[PlanningFailure, B]): Either[PlanningFailure, List[B]] =
    values match {
      case value :: Nil => checkTimeout.flatMap(_ => evaluate(value)).map(List(_))
      case Nil          => checkTimeout.flatMap(_ => Left(PlanningFailure.Rejected("No complete route candidate was found.")))
      case _            =>
        recordCandidates(values.size).flatMap { _ =>
          @tailrec
          def collect(
            remaining: List[A],
            successes: List[B],
            firstFailure: Option[PlanningFailure]
          ): Either[PlanningFailure, List[B]] =
            remaining match {
              case Nil           =>
                if (successes.nonEmpty) Right(successes.reverse)
                else Left(firstFailure.getOrElse(PlanningFailure.Rejected("No complete route candidate was found.")))
              case value :: tail =>
                recordExpansion match {
                  case Left(failure) => Left(failure)
                  case Right(_)      =>
                    evaluate(value) match {
                      case Right(candidate)                         => collect(tail, candidate :: successes, firstFailure)
                      case Left(failure: PlanningFailure.Exhausted) => Left(failure)
                      case Left(failure)                            => collect(tail, successes, firstFailure.orElse(Some(failure)))
                    }
                }
            }

          collect(values, Nil, None)
        }
    }

  private def checkCapacity(count: Long): Either[PlanningFailure, Unit] =
    if (count > limits.maxCandidates.toLong - candidateCount)
      exhausted("Route planning exceeded the configured candidate limit.")
    else checkTimeout

  private def recordCandidates(count: Long): Either[PlanningFailure, Unit] =
    if (count <= 1) checkTimeout
    else
      checkCapacity(count).flatMap { _ =>
        candidateCount += count
        checkTimeout
      }

  private def recordExpansion: Either[PlanningFailure, Unit] =
    if (expansionCount >= limits.maxExpansions)
      exhausted("Route planning exceeded the configured expansion limit.")
    else {
      expansionCount += 1
      checkTimeout
    }

  private def exhausted(message: String): Either[PlanningFailure, Unit] =
    Left(PlanningFailure.Exhausted(message))
}

private[gateway] object CandidateSearch {
  sealed trait PlanningFailure {
    def message: String
  }

  object PlanningFailure {
    final case class Exhausted(message: String) extends PlanningFailure
    final case class Rejected(message: String)  extends PlanningFailure
  }

  final case class Limits(maxCandidates: Int, maxExpansions: Int, timeout: Duration)

}
