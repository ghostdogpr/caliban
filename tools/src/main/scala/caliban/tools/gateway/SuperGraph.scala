package caliban.tools.gateway

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.NullValue
import caliban._
import caliban.execution._
import caliban.introspection.adt.{ __Directive, __Schema, Extend, TypeVisitor }
import caliban.schema.Step.NullStep
import caliban.schema.{ Operation, RootSchemaBuilder, Types }
import caliban.tools.gateway.Resolver.FetchRequest
import caliban.wrappers.Wrapper
import caliban.wrappers.Wrapper.FieldWrapper
import zio.prelude.NonEmptyList
import zio.query.{ DataSource, ZQuery }
import zio.{ Chunk, RIO, URIO, ZIO }

case class SuperGraph[-R](
  protected val subGraphs: List[SubGraph[R]],
  protected val transformers: Chunk[Map[String, __Schema] => TypeVisitor]
) { self =>

  def compose[R1](subGraph: SubGraph[R1]): SuperGraph[R with R1] =
    new SuperGraph[R with R1](subGraph :: self.subGraphs, transformers) {}

  def transform(transformer: TypeVisitor): SuperGraph[R] = {
    val t = (_: Map[String, __Schema]) => transformer
    new SuperGraph(self.subGraphs, self.transformers :+ t)
  }

  private def transformWith(makeTransformer: Map[String, __Schema] => TypeVisitor): SuperGraph[R] =
    new SuperGraph(self.subGraphs, self.transformers :+ makeTransformer)

  def extend(
    sourceGraph: String,
    sourceFieldName: String,
    targetTypeName: String,
    targetFieldName: String,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    filterBatchResults: Option[(Map[String, InputValue], ResponseValue) => Boolean] = None
  ): SuperGraph[R] =
    transformWith(
      _.get(sourceGraph)
        .fold(TypeVisitor.empty)(schema =>
          schema.queryType.allFields.find(_.name == sourceFieldName) orElse
            schema.mutationType.flatMap(_.allFields.find(_.name == sourceFieldName)) orElse
            schema.subscriptionType.flatMap(_.allFields.find(_.name == sourceFieldName)) match {
            case Some(fieldDefinition) =>
              TypeVisitor.fields.addWith(t =>
                if (t.name.contains(targetTypeName))
                  List(
                    fieldDefinition.copy(
                      name = targetFieldName,
                      args = Nil,
                      extend = Some(Extend(sourceGraph, sourceFieldName, argumentMappings, filterBatchResults))
                    )
                  )
                else Nil
              )
            case None                  => TypeVisitor.empty
          }
        )
    )

  def build: RIO[R, GraphQL[R]] =
    for {
      subGraphs   <- ZIO.foreachPar(self.subGraphs)(_.build)
      nel         <- ZIO
                       .succeed(NonEmptyList.fromIterableOption(subGraphs))
                       .someOrFail(new Throwable("At least one subgraph must be defined"))
      subGraphsMap = subGraphs.map(g => g.name -> g.schema).toMap
    } yield SuperGraphQL(nel, transformers.map(_(subGraphsMap)))
}

object SuperGraph {
  val empty: SuperGraph[Any] = new SuperGraph[Any](Nil, Chunk.empty)

  def compose[R](subGraphs: List[SubGraph[R]]): SuperGraph[R] =
    new SuperGraph[R](subGraphs, Chunk.empty)
}

private case class SuperGraphQL[-R](
  private val subGraphs: NonEmptyList[SubGraphData[R]],
  private val transformers: Chunk[TypeVisitor]
) extends GraphQL[R] {
  private val subGraphMap: Map[String, SubGraphData[R]] = subGraphs.map(g => g.name -> g).toMap

  protected val schemaBuilder: RootSchemaBuilder[R] = {
    val builder = subGraphs.collect {
      case subGraph if subGraph.exposeAtRoot =>
        RootSchemaBuilder(
          Some(Operation(subGraph.schema.queryType, NullStep)),
          subGraph.schema.mutationType.map(mutation => Operation(mutation, NullStep)),
          subGraph.schema.subscriptionType.map(subscription => Operation(subscription, NullStep))
        ).visit(
          TypeVisitor.fields.modifyWith((t, field) =>
            if (t.name == subGraph.schema.queryType.name)
              field.copy(extend = Some(Extend(subGraph.name, field.name, Map.empty, None)))
            else field
          // TODO mutation and subscription
          )
        )
    }.reduceLeft(_ |+| _)
    transformers.foldLeft(builder) { case (builder, transformer) => builder.visit(transformer) }
  }

  protected val wrappers: List[Wrapper[R]]              = Nil
  protected val additionalDirectives: List[__Directive] = Nil
  protected val features: Set[Feature]                  = Set.empty

  protected override def resolve[R1 <: R](
    op: Operation[R1],
    fieldWrappers: List[FieldWrapper[R1]], // TODO field wrappers
    isIntrospection: Boolean
  )(req: ExecutionRequest): URIO[R1, GraphQLResponse[CalibanError]] =
    if (isIntrospection)
      Executor.executeRequest(req, op.plan, fieldWrappers, QueryExecution.Parallel, features)
    else {
      val a = makeResolver(req.field)
      resolveField(a, NullValue)
        .fold(
          error => GraphQLResponse(NullValue, List(error)),
          result => GraphQLResponse(result, Nil)
        )
        .run
    }

  private def makeResolver(field: caliban.execution.Field): Resolver.Field =
    Resolver.Field(
      field.name,
      field.definition.flatMap(_.extend) match {
        case Some(extend) =>
          Resolver.Fetch(
            extend.sourceGraph,
            sourceFieldName = extend.sourceFieldName,
            fields = field.fields.map(makeResolver),
            argumentMappings = extend.argumentMappings,
            filterBatchResults = extend.filterBatchResults
          )
        case None         =>
          val extract: ObjectValue => ResponseValue = if (field.isRoot) identity else _.get(field.name)
          Resolver.Extract(extract, field.fields.map(makeResolver))
      },
      field.alias,
      field.arguments
    )

  private def resolveField(field: Resolver.Field, parent: ResponseValue): ZQuery[R, ExecutionError, ResponseValue] =
    field.resolver match {
      case Resolver.Extract(extract, children)                                                           =>
        extract(parent.asObjectValue.getOrElse(ObjectValue(Nil))) match {
          case res @ ObjectValue(fields) =>
            ZQuery
              .foreachBatched(children)(child => resolveField(child, res).map(child -> _))
              .map(children =>
                res.copy(fields = fields ++ children.map { case (field, response) =>
                  field.alias.getOrElse(field.name) -> response
                })
              )
          case other                     => ZQuery.succeed(other)
        }
      case Resolver.Fetch(subGraph, sourceFieldName, fields, argumentMappings, mapBatchResultToArgument) =>
        subGraphMap.get(subGraph) match {
          case Some(subGraph) =>
            val arguments = field.arguments ++ argumentMappings.map { case (k, f) =>
              f(parent.asObjectValue.map(_.get(k)).map(_.toInputValue).getOrElse(NullValue))
            }.filterNot { case (_, v) => v == NullValue }
            ZQuery
              .fromRequest(
                FetchRequest(
                  subGraph,
                  sourceFieldName,
                  fields
                    .flatMap(f =>
                      f.resolver match {
                        case _: Resolver.Extract                          => Set(f.name)
                        case Resolver.Fetch(_, _, _, argumentMappings, _) => argumentMappings.keySet
                      }
                    )
                    .distinct
                    .map(name => caliban.execution.Field(name, Types.string, None)),
                  arguments
                )
              )(Resolver.dataSource[R]) // TODO don't make new datasource for each request
              .map(_.asObjectValue.map(_.get(sourceFieldName)).getOrElse(NullValue))
              .map {
                case ListValue(values) =>
                  ListValue(values.filter(v => mapBatchResultToArgument.fold(true)(_(arguments, v))))
                case other             => other
              }
              .flatMap {
                case ListValue(values) =>
                  ZQuery
                    .foreachBatched(values)(value =>
                      ZQuery
                        .foreachBatched(fields)(field =>
                          resolveField(field, value).map(field.alias.getOrElse(field.name) -> _)
                        )
                        .map(ObjectValue.apply)
                    )
                    .map(ListValue.apply)
                case value             =>
                  ZQuery
                    .foreachBatched(fields)(field =>
                      resolveField(field, value).map(field.alias.getOrElse(field.name) -> _)
                    )
                    .map(ObjectValue.apply)
              }
          case None           => ZQuery.fail(ExecutionError(s"Subgraph $subGraph not found"))
        }
    }
}

sealed trait Resolver
object Resolver {
  case class Extract(extract: ObjectValue => ResponseValue, fields: List[Field] = Nil) extends Resolver
  case class Fetch(
    subGraph: String,
    sourceFieldName: String,
    fields: List[Field],
    argumentMappings: Map[String, InputValue => (String, InputValue)] = Map.empty,
    filterBatchResults: Option[(Map[String, InputValue], ResponseValue) => Boolean] = None
  ) extends Resolver

  case class Field(
    name: String,
    resolver: Resolver,
    alias: Option[String] = None,
    arguments: Map[String, InputValue] = Map.empty
  )

  val test: Field =
    Field(
      "",
      Extract(
        identity,
        List(
          Field(
            "stores",
            Fetch(
              "Stores",
              "stores",
              List(
                Field("id", Extract(_.get("id"))),
                Field("name", Extract(_.get("name"))),
                Field("name", Extract(_.get("name")), Some("name2")),
                Field(
                  "bookSells",
                  Fetch(
                    "Stores",
                    "bookSells",
                    argumentMappings = Map("id" -> ("storeId" -> _)),
                    fields = List(
                      Field("sellsCount", Extract(_.get("sellsCount"))),
                      Field(
                        "book",
                        Fetch(
                          "Books",
                          "book",
                          argumentMappings = Map("bookId" -> ("id" -> _)),
                          fields = List(
                            Field("id", Extract(_.get("id"))),
                            Field("title", Extract(_.get("title"))),
                            Field(
                              "author",
                              Fetch(
                                "Authors",
                                "authors_v1_AuthorsService_GetAuthors",
                                argumentMappings = Map(
                                  "authorId" -> (v =>
                                    "input" -> InputValue.ObjectValue(Map("ids" -> InputValue.ListValue(List(v))))
                                  )
                                ),
                                filterBatchResults = Some((arguments, responseValue) =>
                                  arguments
                                    .get("input")
                                    .flatMap(_.asInputObjectValue)
                                    .flatMap(_.fields.get("ids"))
                                    .flatMap(_.asInputListValue)
                                    .exists(
                                      _.values.contains(
                                        responseValue.asObjectValue.map(_.get("id").toInputValue).getOrElse(NullValue)
                                      )
                                    )
                                ),
                                fields = List(Field("id", Extract(_.get("id"))), Field("name", Extract(_.get("name"))))
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                Field(
                  "bookSells",
                  Fetch(
                    "Stores",
                    "bookSells",
                    argumentMappings = Map("id" -> ("storeId" -> _)),
                    fields = List(
                      Field("sellsCount", Extract(_.get("sellsCount")))
                    )
                  ),
                  Some("bookSells2")
                )
              )
            )
          )
        )
      )
    )

  case class FetchRequest[-R](
    subGraph: SubGraphData[R],
    sourceFieldName: String,
    fields: List[caliban.execution.Field],
    arguments: Map[String, InputValue]
  ) extends zio.query.Request[ExecutionError, ResponseValue]

  def dataSource[R]: DataSource[R, FetchRequest[R]] =
    DataSource.fromFunctionBatchedZIO[R, ExecutionError, FetchRequest[R], ResponseValue]("RemoteDataSource") {
      requests =>
        val requestsMap = requests.groupBy(_.subGraph).flatMap { case (subGraph, requests) =>
          val subGraphRequest: (SubGraphData[R], (caliban.execution.Field, Map[FetchRequest[R], String])) =
            subGraph -> requests
              .groupBy(_.sourceFieldName)
              .foldLeft(caliban.execution.Field("", Types.string, None) -> Map.empty[FetchRequest[R], String]) {
                case ((field, rootFieldMap), (sourceFieldName, requests)) =>
                  val (mergedFields, updatedRootFieldMap) = {
                    val fields                 = requests
                      .map(request =>
                        request -> caliban.execution.Field(
                          sourceFieldName,
                          Types.string,
                          None,
                          fields = request.fields,
                          arguments = request.arguments
                        )
                      )
                    val (firstReq, firstField) = fields.head
                    fields.tail.foldLeft((List(firstField), Map(firstReq -> sourceFieldName))) {
                      case ((fields, rootFieldMap), (request, field)) =>
                        val (merged, res) = fields
                          .foldLeft((false, List.empty[caliban.execution.Field])) { case ((merged, res), f) =>
                            if (merged) (merged, f :: res)
                            else
                              combineFieldArguments(field, f) // TODO also combine subfields
                                .fold((false, f :: res))(merged =>
                                  (
                                    true,
                                    merged.copy(alias = f.alias, fields = (f.fields ++ field.fields).distinct) :: res
                                  )
                                )
                          }
                        if (merged) (res, rootFieldMap.updated(request, field.name))
                        else {
                          val alias = s"${field.name}${res.size}"
                          (field.copy(alias = Some(alias)) :: res, rootFieldMap.updated(request, alias))
                        }
                    }
                  }
                  field.copy(fields = mergedFields ++ field.fields) -> (rootFieldMap ++ updatedRootFieldMap)
              }

          requests.map(_ -> subGraphRequest)
        }

        ZIO
          .foreachPar(Chunk.fromIterable(requestsMap.values).distinct) { case key @ (subGraph, (field, _)) =>
            subGraph.run(field).map(key -> _)
          }
          .map(_.toMap)
          .map(results =>
            requests.flatMap(req =>
              requestsMap.get(req).flatMap { case key @ (_, (_, rootFieldMap)) =>
                results.get(key).map {
                  case ObjectValue(fields) =>
                    val fieldName = rootFieldMap.get(req)
                    ObjectValue(fields.collect {
                      case (k, v) if fieldName.contains(k) => req.sourceFieldName -> v
                    })
                  case other               => other
                }
              }
            )
          )
    }

  private def combineFieldArguments(
    f1: caliban.execution.Field,
    f2: caliban.execution.Field
  ): Option[caliban.execution.Field] =
    mergeInputValueMaps(f1.arguments, f2.arguments).flatMap { mergedArguments =>
      import zio.prelude._
      (f1.fields zip f2.fields).forEach { case (f1, f2) => combineFieldArguments(f1, f2) }.map { mergedFields =>
        f1.copy(arguments = mergedArguments, fields = mergedFields)
      }
    }

  private def mergeInputValueMaps(
    m1: Map[String, InputValue],
    m2: Map[String, InputValue]
  ): Option[Map[String, InputValue]] = {
    val keys = m1.keySet ++ m2.keySet
    keys.foldLeft(Option(Map.empty[String, InputValue])) {
      case (None, _)        => None
      case (Some(acc), key) =>
        (m1.get(key), m2.get(key)) match {
          case (Some(i1), Some(i2)) =>
            (i1, i2) match {
              case (InputValue.ListValue(v1), InputValue.ListValue(v2))     =>
                Some(acc.updated(key, InputValue.ListValue((v1 ++ v2).distinct)))
              case (InputValue.ObjectValue(v1), InputValue.ObjectValue(v2)) =>
                mergeInputValueMaps(v1, v2).map(merged => acc.updated(key, InputValue.ObjectValue(merged)))
              case _                                                        =>
                if (i1 == i2) Some(acc.updated(key, i1))
                else None
            }
          case _                    => None
        }
    }
  }
}
