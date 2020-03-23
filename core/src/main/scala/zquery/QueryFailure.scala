package zquery

import zio.Cause

/**
 * `QueryFailure` keeps track of details relevant to query failures.
 */
final case class QueryFailure(dataSource: DataSource[Nothing, Nothing], request: Request[Any, Any])
    extends Throwable(null, null, true, false) {
  override def getMessage: String =
    s"Data source ${dataSource.identifier} did not complete request ${request.toString}."
}

object QueryFailure {

  /**
   * Strips all query failures from the specified cause, returning either
   * `None` is there are no causes other than query failures or a cause known
   * to contain no query failures.
   */
  def strip[E](cause: Cause[E]): Option[Cause[E]] =
    cause.fold(
      None,
      e => Some(Cause.Fail(e)), {
        case _: QueryFailure => None
        case t               => Some(Cause.Die(t))
      },
      fiberId => Some(Cause.Interrupt(fiberId))
    )(
      {
        case (Some(l), Some(r)) => Some(Cause.Both(l, r))
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      }, {
        case (Some(l), Some(r)) => Some(Cause.Then(l, r))
        case (Some(l), None)    => Some(l)
        case (None, Some(r))    => Some(r)
        case (None, None)       => None
      },
      (causeOption, trace) => causeOption.map(Cause.Traced(_, trace))
    )
}
