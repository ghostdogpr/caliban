package caliban

import zio.{ FiberRef, UIO, Unsafe, ZIO }

private[caliban] object GraphQLResponseContext {

  sealed trait Outcome

  object Outcome {
    case object Executed        extends Outcome
    case object Subscribed      extends Outcome
    case object RequestError    extends Outcome
    case object MutationOverGet extends Outcome
  }

  sealed trait ServerFailure extends Outcome

  object ServerFailure {
    case object Internal    extends ServerFailure
    case object Unavailable extends ServerFailure
    case object TimedOut    extends ServerFailure
  }

  private val ExecutedMark   = Some(Outcome.Executed)
  private val SubscribedMark = Some(Outcome.Subscribed)

  private val current: FiberRef[Option[Outcome]] =
    Unsafe.unsafe(implicit unsafe => FiberRef.unsafe.make(None))

  def capture[R, E, A, B](effect: ZIO[R, E, GraphQLResponse[A]])(f: (GraphQLResponse[A], Outcome) => B): ZIO[R, E, B] =
    current.locally(None)(effect.zipWith(current.get) { (response, marked) =>
      f(response, marked.getOrElse(outcome(response)))
    })

  def outcome(response: GraphQLResponse[Any]): Outcome =
    response.errors.collectFirst(requestOutcome).getOrElse(Outcome.Executed)

  def markRequestError(error: CalibanError): UIO[Unit] =
    requestOutcome.lift(error).fold(ZIO.unit)(outcome => current.set(Some(outcome)))

  def markServerError(failure: ServerFailure): UIO[Unit] =
    current.set(Some(failure))

  def markExecuted: UIO[Unit] =
    current.set(ExecutedMark)

  def markSubscribed: UIO[Unit] =
    current.set(SubscribedMark)

  private val requestOutcome: PartialFunction[Any, Outcome] = {
    case HttpUtils.MutationOverGetError                                 => Outcome.MutationOverGet
    case _: CalibanError.ParsingError | _: CalibanError.ValidationError => Outcome.RequestError
  }
}
