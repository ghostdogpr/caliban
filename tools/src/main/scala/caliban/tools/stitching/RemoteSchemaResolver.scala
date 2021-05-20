package caliban.tools.stitching

import zio._
import zio.query._

import caliban.GraphQLResponse
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema._
import caliban.{ CalibanError, ResponseValue }
import CalibanError.ExecutionError

case class RemoteSchemaResolver(schema: __Schema, typeMap: Map[String, __Type], apiURL: String) {
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

  def proxy[R](
    resolver: RemoteResolver[R, ExecutionError, Field, ResponseValue]
  ): RootSchemaBuilder[R] = RootSchemaBuilder(
    query = Some(
      Operation[R](
        schema.queryType,
        Step.MetadataFunctionStep((args: caliban.execution.Field) =>
          Step.QueryStep(
            ZQuery.fromEffect(resolver.run(args)).map(Step.PureStep)
          )
        )
      )
    ),
    mutation = None,
    subscription = None
  )
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

    RemoteSchemaResolver(schema, typeMap, apiURL)
  }
}
