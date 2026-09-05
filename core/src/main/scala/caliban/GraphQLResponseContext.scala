package caliban

import zio.{ FiberRef, UIO, Unsafe, ZIO }

private[caliban] object GraphQLResponseContext {

  sealed trait Outcome

  object Outcome {
    case object Executed                                 extends Outcome
    case object RequestError                             extends Outcome
    final case class ServerError(failure: ServerFailure) extends Outcome
  }

  sealed trait ServerFailure

  object ServerFailure {
    case object Internal    extends ServerFailure
    case object Unavailable extends ServerFailure
    case object TimedOut    extends ServerFailure
  }

  final case class Classified[+A](value: A, outcome: Outcome)

  private final case class Classification(outcome: Outcome, definitive: Boolean)

  private val initial = Classification(Outcome.Executed, definitive = false)

  private val current: FiberRef[Classification] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(initial))

  def capture[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, Classified[A]] =
    captureWith(effect)((value, state) => Classified(value, state.outcome))

  def captureResponse[R, E, A](
    effect: ZIO[R, E, GraphQLResponse[A]]
  ): ZIO[R, E, Classified[GraphQLResponse[A]]] =
    captureWith(effect) { (response, outcome) =>
      val classifiedOutcome =
        if (!outcome.definitive && response.errors.exists(isRequestError)) Outcome.RequestError
        else outcome.outcome
      Classified(response, classifiedOutcome)
    }

  private def captureWith[R, E, A, B](effect: ZIO[R, E, A])(f: (A, Classification) => B): ZIO[R, E, B] =
    current.locally(initial)(effect.zipWith(current.get)(f))

  def markRequestError(error: CalibanError): UIO[Unit] =
    if (isRequestError(error)) current.set(Classification(Outcome.RequestError, definitive = true))
    else ZIO.unit

  def markServerError(failure: ServerFailure): UIO[Unit] =
    current.set(Classification(Outcome.ServerError(failure), definitive = true))

  def markExecuted: UIO[Unit] =
    current.set(Classification(Outcome.Executed, definitive = true))

  private def isRequestError(error: Any): Boolean =
    error match {
      case _: CalibanError.ParsingError | _: CalibanError.ValidationError => true
      case _                                                              => false
    }
}
