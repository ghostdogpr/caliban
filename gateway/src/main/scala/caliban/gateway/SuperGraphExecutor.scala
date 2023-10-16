package caliban.gateway

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.NullValue
import caliban.execution._
import caliban.gateway.FetchDataSource.FetchRequest
import caliban.gateway.SubGraph.SubGraphExecutor
import caliban.introspection.adt.{ __Directive, Extend, TypeVisitor }
import caliban.parsing.adt.OperationType
import caliban.schema.Step.NullStep
import caliban.schema.{ Operation, RootSchemaBuilder, Types }
import caliban.wrappers.Wrapper
import caliban.wrappers.Wrapper.FieldWrapper
import caliban.{ CalibanError, GraphQL, GraphQLResponse, ResponseValue }
import zio.prelude.NonEmptyList
import zio.query.ZQuery
import zio.{ Chunk, URIO }

private case class SuperGraphExecutor[-R](
  private val subGraphs: NonEmptyList[SubGraphExecutor[R]],
  private val transformers: Chunk[TypeVisitor]
) extends GraphQL[R] {
  private val subGraphMap: Map[String, SubGraphExecutor[R]] = subGraphs.map(g => g.name -> g).toMap

  protected val wrappers: List[Wrapper[R]]              = Nil
  protected val additionalDirectives: List[__Directive] = Nil
  protected val features: Set[Feature]                  = Set.empty
  protected val schemaBuilder: RootSchemaBuilder[R]     = {
    val builder = subGraphs.collect {
      case subGraph if subGraph.exposeAtRoot =>
        val rootTypes = Set(
          subGraph.schema.queryType.name,
          subGraph.schema.mutationType.flatMap(_.name),
          subGraph.schema.subscriptionType.flatMap(_.name)
        ).flatten
        RootSchemaBuilder(
          Some(Operation(subGraph.schema.queryType, NullStep)),
          subGraph.schema.mutationType.map(mutation => Operation(mutation, NullStep)),
          subGraph.schema.subscriptionType.map(subscription => Operation(subscription, NullStep))
        ).visit(
          TypeVisitor.fields.modifyWith((t, field) =>
            if (t.name.exists(rootTypes.contains)) field.copy(extend = Some(Extend(subGraph.name, field.name)))
            else field
          )
        )
    }.reduceLeft(_ |+| _)
    transformers.foldLeft(builder) { case (builder, transformer) => builder.visit(transformer) }
  }

  protected override def resolve[R1 <: R](
    op: Operation[R1],
    fieldWrappers: List[FieldWrapper[R1]],
    isIntrospection: Boolean
  )(req: ExecutionRequest): URIO[R1, GraphQLResponse[CalibanError]] =
    if (isIntrospection)
      Executor.executeRequest(req, op.plan, fieldWrappers, QueryExecution.Parallel, features)
    else
      resolveField(Resolver.Field(req.field), req.operationType)
        .fold(
          error => GraphQLResponse(NullValue, List(error)),
          result => GraphQLResponse(result, Nil)
        )
        .run

  private def resolveField(
    field: Resolver.Field,
    operationType: OperationType
  ): ZQuery[R, ExecutionError, ResponseValue] = {
    val dataSource = FetchDataSource[R]

    def foreach[A, B](resolvers: List[A])(f: A => ZQuery[R, ExecutionError, B]): ZQuery[R, ExecutionError, List[B]] =
      operationType match {
        case OperationType.Query | OperationType.Subscription => ZQuery.foreachBatched(resolvers)(f)
        case OperationType.Mutation                           => ZQuery.foreach(resolvers)(f)
      }

    def resolve(field: Resolver.Field, parent: ResponseValue): ZQuery[R, ExecutionError, ResponseValue] =
      field.resolver match {
        case extractor: Resolver.Extract => resolveExtractor(extractor, parent)
        case fetcher: Resolver.Fetch     => resolveFetcher(fetcher, parent)
      }

    def resolveExtractor(extractor: Resolver.Extract, parent: ResponseValue): ZQuery[R, ExecutionError, ResponseValue] =
      extractor.extract(parent.asObjectValue) match {
        case res @ ObjectValue(fields) =>
          foreach(extractor.fields)(child => resolve(child, res).map(child -> _))
            .map(children =>
              res.copy(fields = fields ++ children.map { case (field, response) => field.outputName -> response })
            )
        case other                     => ZQuery.succeed(other)
      }

    def resolveFetcher(fetcher: Resolver.Fetch, parent: ResponseValue): ZQuery[R, ExecutionError, ResponseValue] =
      subGraphMap.get(fetcher.subGraph) match {
        case Some(subGraph) =>
          lazy val parentObject = parent.asObjectValue
          val arguments         = field.arguments ++ fetcher.argumentMappings.map { case (k, f) =>
            f(parentObject.get(k).toInputValue)
          }.filterNot { case (_, v) => v == NullValue }
          ZQuery
            .fromRequest(
              FetchRequest(
                subGraph,
                fetcher.sourceFieldName,
                operationType,
                fetcher.fields
                  .flatMap(f =>
                    f.resolver match {
                      case _: Resolver.Extract                          => Set(f.name)
                      case Resolver.Fetch(_, _, _, argumentMappings, _) => argumentMappings.keySet
                    }
                  )
                  .distinct
                  .map(name => Field(name, Types.string, None)),
                arguments,
                fetcher.filterBatchResults.isDefined
              )
            )(dataSource)
            .flatMap {
              case ListValue(values) =>
                val filteredValues = fetcher.filterBatchResults match {
                  case Some(filter) => values.filter(value => filter(parentObject, value.asObjectValue))
                  case _            => values
                }
                foreach(filteredValues)(resolveObject(fetcher.fields, _)).map(ListValue.apply)
              case value             =>
                resolveObject(fetcher.fields, value)
            }
        case None           => ZQuery.fail(ExecutionError(s"Subgraph ${fetcher.subGraph} not found"))
      }

    def resolveObject(fields: List[Resolver.Field], value: ResponseValue): ZQuery[R, ExecutionError, ObjectValue] =
      foreach(fields)(field => resolve(field, value).map(field.outputName -> _)).map(ObjectValue.apply)

    resolve(field, NullValue)
  }
}
