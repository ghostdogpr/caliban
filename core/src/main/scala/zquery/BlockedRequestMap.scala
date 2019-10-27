package zquery

import zio.ZIO

/**
 * A `BlockedRequestMap[R, E]` maintains a mapping from data sources to
 * requests from those data sources.
 */
private[zquery] final class BlockedRequestMap[-R, +E](
  private val map: Map[DataSource.Service[Any, Nothing, Any], Vector[BlockedRequest[Any]]]
) { self =>

  def ++[R1 <: R, E1 >: E](that: BlockedRequestMap[R1, E1]): BlockedRequestMap[R1, E1] =
    new BlockedRequestMap(
      (self.map.toVector ++ that.map.toVector)
        .foldLeft[Map[DataSource.Service[Any, Nothing, Any], Vector[BlockedRequest[Any]]]](Map()) {
          case (acc, (key, value)) =>
            acc + (key -> acc.get(key).fold(value)(_ ++ value))
        }
    )

  /**
   * Transforms all data sources with the specified data source function, which
   * can change the environment and error types of data sources but must
   * preserve the request type of each data source.
   */
  final def mapDataSources[R1, E1](f: DataSourceFunction[R, E, R1, E1]): BlockedRequestMap[R1, E1] =
    new BlockedRequestMap(self.map.map { case (k, v) => (f(k).asInstanceOf[DataSource.Service[Any, Nothing, Any]], v) })

  /**
   * Executes all requests, submitting batched requests to each data source in
   * parallel.
   */
  val run: ZIO[R, E, Unit] =
    ZIO.foreachPar_(map) {
      case (dataSource, blockedRequests) =>
        for {
          completedRequests <- dataSource.run(blockedRequests.map(_.request))
          _ <- ZIO.foreach_(blockedRequests) { blockedRequest =>
                blockedRequest.result.set(completedRequests.lookup(blockedRequest.request))
              }
        } yield ()
    }
}

object BlockedRequestMap {

  /**
   * Constructs a new blocked request map containing a mapping from the
   * specified data source to the specified request.
   */
  def apply[R, E, K](
    dataSource: DataSource.Service[R, E, K],
    blockedRequest: BlockedRequest[K]
  ): BlockedRequestMap[R, E] =
    new BlockedRequestMap(
      Map(dataSource.asInstanceOf[DataSource.Service[Any, Nothing, Any]] -> Vector(blockedRequest))
    )

  /**
   * An empty blocked requests map.
   */
  val empty: BlockedRequestMap[Any, Nothing] =
    new BlockedRequestMap(
      Map.empty[DataSource.Service[Any, Nothing, Any], Vector[BlockedRequest[Any]]]
    )
}
