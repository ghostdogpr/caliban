package caliban.tools.stitching

import zio._
import zio.query._

import caliban.GraphQL
import caliban.GraphQLResponse
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema._
import caliban.{ CalibanError, ResponseValue }
import CalibanError.ExecutionError

case class RemoteSchemaResolver(schema: __Schema, typeMap: Map[String, __Type]) {
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
    resolver: RemoteResolver[R, ExecutionError, Field, ResponseValue],
    mutationResolver: Option[RemoteResolver[R, ExecutionError, Field, ResponseValue]]
  ): GraphQL[R] = {
    def toOperation(
      rootType: Option[__Type],
      resolver: Option[RemoteResolver[R, ExecutionError, Field, ResponseValue]]
    ): Option[Operation[R]] =
      (rootType zip resolver).headOption
        .map({ case (rootType, resolver) =>
          Operation[R](
            rootType,
            Step.ObjectStep(
              rootType.name.getOrElse(""),
              rootType
                .fields(__DeprecatedArgs(Some(true)))
                .getOrElse(List())
                .map { field =>
                  (field.name ->
                    Step.MetadataFunctionStep((args: caliban.execution.Field) =>
                      Step.QueryStep(
                        ZQuery.fromEffect(resolver.run(args)).map(Step.PureStep)
                      )
                    ))
                }
                .toMap
            )
          )
        })

    val builder = RootSchemaBuilder(
      query = toOperation(Option(schema.queryType), Option(resolver)),
      mutation = toOperation(schema.mutationType, mutationResolver),
      subscription = None
    )

    new GraphQL[R] {
      protected val additionalDirectives: List[__Directive]            = schema.directives
      protected val schemaBuilder: caliban.schema.RootSchemaBuilder[R] = builder
      protected val wrappers: List[caliban.wrappers.Wrapper[R]]        = List()
    }
  }
}

object RemoteSchemaResolver {
  def fromSchema(schema: __Schema): RemoteSchemaResolver = {
    val typeMap = schema.types
      .collect({ t =>
        (t.name) match {
          case Some(name) => name -> t
        }
      })
      .toMap

    RemoteSchemaResolver(schema, typeMap)
  }
}
