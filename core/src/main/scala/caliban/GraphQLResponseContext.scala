package caliban

import zio.{ FiberRef, UIO, Unsafe, ZIO }

private[caliban] object GraphQLResponseContext {

  sealed trait Outcome

  object Outcome {
    case object Executed                          extends Outcome
    case object RequestError                      extends Outcome
    case object MethodNotAllowed                  extends Outcome
    final case class ServerError(statusCode: Int) extends Outcome
  }

  final case class Classified[+A](value: A, outcome: Outcome)

  private val current: FiberRef[Outcome] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(Outcome.Executed))

  def capture[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, Classified[A]] =
    current.locally(Outcome.Executed)(effect.zipWith(current.get)(Classified(_, _)))

  def markRequestError(error: CalibanError): UIO[Unit] =
    current.set(
      if (error == HttpUtils.MutationOverGetError) Outcome.MethodNotAllowed
      else Outcome.RequestError
    )

  def markServerError(statusCode: Int): UIO[Unit] =
    current.set(Outcome.ServerError(statusCode))
}
