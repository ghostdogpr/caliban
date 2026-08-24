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

  private val current: FiberRef[Outcome] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(Outcome.Executed))

  def capture[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, Classified[A]] =
    current.locally(Outcome.Executed)(effect.zipWith(current.get)(Classified(_, _)))

  def markRequestError(_error: CalibanError): UIO[Unit] =
    current.set(Outcome.RequestError)

  def markServerError(failure: ServerFailure): UIO[Unit] =
    current.set(Outcome.ServerError(failure))
}
