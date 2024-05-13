package caliban.tools.stitching

import caliban.CalibanError.ExecutionError
import caliban.execution.{ Feature, Field }
import caliban.introspection.adt._
import caliban.schema._
import caliban.transformers.Transformer
import caliban.{ CalibanError, GraphQL, ResponseValue }
import zio._
import zio.query._

case class RemoteSchemaResolver(schema: __Schema, typeMap: Map[String, __Type]) {
  def remoteResolver[R, R0, A](typeName: String)(
    resolver: RemoteResolver[R0, CalibanError.ExecutionError, ResolveRequest[A], ResponseValue]
  ): PartialRemoteSchema[R0, R, A] = new PartialRemoteSchema[R0, R, A] {
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
      (rootType zip resolver).headOption.map { case (rootType, resolver) =>
        Operation[R](
          rootType,
          Step.ObjectStep(
            rootType.name.getOrElse(""),
            rootType.allFields.map { field =>
              field.name ->
                Step.MetadataFunctionStep((args: caliban.execution.Field) =>
                  Step.QueryStep(
                    ZQuery.fromZIO(resolver.run(args)).map(Step.PureStep.apply)
                  )
                )
            }.toMap
          )
        )
      }

    val builder = RootSchemaBuilder(
      query = toOperation(Option(schema.queryType), Option(resolver)),
      mutation = toOperation(schema.mutationType, mutationResolver),
      subscription = None
    )

    new GraphQL[R] {
      override protected val additionalDirectives: List[__Directive]            = schema.directives
      override protected val schemaBuilder: caliban.schema.RootSchemaBuilder[R] = builder
      override protected val wrappers: List[caliban.wrappers.Wrapper[R]]        = List()
      override protected val features: Set[Feature]                             = Set.empty
      override protected val transformer: Transformer[R]                        = Transformer.empty
    }
  }
}

object RemoteSchemaResolver {
  def fromSchema(schema: __Schema): RemoteSchemaResolver = {
    val typeMap = schema.types
      .collect({ t =>
        t.name match {
          case Some(name) => name -> t
        }
      })
      .toMap

    RemoteSchemaResolver(schema, typeMap)
  }
}
