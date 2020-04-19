package zquery

import zio.ZIO

/**
 * A `BlockedRequestMap[R]` maintains a mapping from data sources to
 * requests from those data sources.
 */
private[zquery] final class BlockedRequestMap[-R](
  private val map: Map[DataSource[Any, Any], Vector[BlockedRequest[Any]]]
) { self =>

  def ++[R1 <: R](that: BlockedRequestMap[R1]): BlockedRequestMap[R1] =
    new BlockedRequestMap(
      (self.map.toVector ++ that.map.toVector)
        .foldLeft[Map[DataSource[Any, Any], Vector[BlockedRequest[Any]]]](Map()) {
          case (acc, (key, value)) =>
            acc + (key -> acc.get(key).fold(value)(_ ++ value))
        }
    )

  /**
   * Transforms all data sources with the specified data source function, which
   * can change the environment and error types of data sources but must
   * preserve the request type of each data source.
   */
  def mapDataSources[R1](f: DataSourceFunction[R, R1]): BlockedRequestMap[R1] =
    new BlockedRequestMap(self.map.map { case (k, v) => (f(k).asInstanceOf[DataSource[Any, Any]], v) })

  /**
   * Executes all requests, submitting batched requests to each data source in
   * parallel.
   */
  val run: ZIO[R, Nothing, Unit] =
    ZIO.foreachPar_(map) {
      case (dataSource, blockedRequests) =>
        for {
          completedRequests <- dataSource.run(blockedRequests.map(_.request).toSet)
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
    dataSource: DataSource[R, K],
    blockedRequest: BlockedRequest[K]
  ): BlockedRequestMap[R] =
    new BlockedRequestMap(
      Map(dataSource.asInstanceOf[DataSource[Any, Any]] -> Vector(blockedRequest))
    )

  /**
   * An empty blocked requests map.
   */
  val empty: BlockedRequestMap[Any] =
    new BlockedRequestMap(
      Map.empty[DataSource[Any, Any], Vector[BlockedRequest[Any]]]
    )
}
