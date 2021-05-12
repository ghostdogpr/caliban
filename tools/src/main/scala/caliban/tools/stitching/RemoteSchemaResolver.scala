package caliban.tools.stitching

import zio._
import zio.query._

import caliban.{ CalibanError, ResponseValue }

import caliban.introspection.adt._

case class RemoteSchemaResolver(typeMap: Map[String, __Type], apiURL: String) {
  val resolvers = new DefaultResolvers(apiURL)

  def remoteResolver[R, R0 <: Has[_], A](typeName: String)(
    resolver: RemoteResolver[
      R0,
      CalibanError.ExecutionError,
      ResolveRequest[A],
      ResponseValue
    ]
  ) = new PartialRemoteSchema[R0, R, A] {
    def resolve(a: A, args: caliban.execution.Field): ZIO[R0, CalibanError, ResponseValue] =
      resolver.run(ResolveRequest(a, args))

    def toType(isInput: Boolean, isSubscription: Boolean): __Type = typeMap(typeName)
  }
}

object RemoteSchemaResolver {
  def fromSchema(schema: __Schema, apiURL: String): RemoteSchemaResolver = {
    val typeMap = schema.types
      .collect({ t =>
        (t.name) match {
          case Some(name) => name -> t
        }
      })
      .toMap

    RemoteSchemaResolver(typeMap, apiURL)
  }
}
