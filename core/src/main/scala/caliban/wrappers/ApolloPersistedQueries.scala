package caliban.wrappers

import caliban.CalibanError.ValidationError
import caliban.Value.{ NullValue, StringValue }
import caliban.wrappers.Wrapper.OverallWrapper
import caliban.{ GraphQLRequest, GraphQLResponse, InputValue }
import zio.{ Has, Layer, Ref, UIO, ZIO, ZLayer }

object ApolloPersistedQueries {

  type ApolloPersistence = Has[Service]

  trait Service {
    def get(hash: String): UIO[Option[String]]
    def add(hash: String, query: String): UIO[Unit]
  }

  object Service {
    val live: UIO[Service] = Ref.make[Map[String, String]](Map()).map { cache =>
      new Service {
        override def get(hash: String): UIO[Option[String]]      = cache.get.map(_.get(hash))
        override def add(hash: String, query: String): UIO[Unit] = cache.update(_.updated(hash, query))
      }
    }
  }

  val live: Layer[Nothing, ApolloPersistence] = ZLayer.fromEffect(Service.live)

  /**
   * Returns a wrapper that persists and retrieves queries based on a hash
   * following Apollo Persisted Queries spec: https://github.com/apollographql/apollo-link-persisted-queries.
   */
  val apolloPersistedQueries: OverallWrapper[ApolloPersistence] =
    OverallWrapper { process => (request: GraphQLRequest) =>
      readHash(request) match {
        case Some(hash) =>
          ZIO
            .accessM[ApolloPersistence](_.get.get(hash))
            .flatMap {
              case Some(query) => UIO(request.copy(query = Some(query)))
              case None        =>
                request.query match {
                  case Some(value) => ZIO.accessM[ApolloPersistence](_.get.add(hash, value)).as(request)
                  case None        => ZIO.fail(ValidationError("PersistedQueryNotFound", ""))
                }

            }
            .flatMap(process)
            .catchAll(ex => UIO(GraphQLResponse(NullValue, List(ex))))
        case None       => process(request)
      }
    }

  private def readHash(request: GraphQLRequest): Option[String] =
    request.extensions
      .flatMap(_.get("persistedQuery"))
      .flatMap {
        case InputValue.ObjectValue(fields) =>
          fields.get("sha256Hash").collectFirst { case StringValue(hash) =>
            hash
          }
        case _                              => None
      }
}
