package caliban.tools

import caliban.CalibanError.ValidationError
import caliban.Value.StringValue
import caliban._
import caliban.execution.Field
import caliban.introspection.adt.{ __Type, __TypeKind }
import caliban.schema.Step.{ MetadataFunctionStep, QueryStep }
import caliban.schema.{ PureStep, Schema, Step, Types }
import caliban.tools.RemoteDataSource.ProxyRequest
import caliban.transformers.Transformer
import zio.query.ZQuery
import zio.{ Task, ZIO }

trait Source[-R] {
  self =>
  def toGraphQL: Task[GraphQL[R]]

  def |+|[R1](anotherSource: Source[R1]): Source[R with R1] =
    new Source[R with R1] {
      def toGraphQL: Task[GraphQL[R with R1]] =
        self.toGraphQL.zipWithPar(anotherSource.toGraphQL)(_ |+| _)
    }

  def transform[R1 <: R](transformer: Transformer[R1]): Source[R1] =
    new Source[R1] {
      def toGraphQL: Task[GraphQL[R1]] = self.toGraphQL.map(_.transform(transformer))
    }

  def extend[R1](
    source: Source[R1],
    sourceFieldName: String,
    targetTypeName: String,
    targetFieldName: String,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    mapBatchResultToArguments: PartialFunction[ResponseValue, Map[String, ResponseValue]] = PartialFunction.empty
  ): Source[R with R1 with SttpClient] = new Source[R with R1 with SttpClient] {
    def toGraphQL: Task[GraphQL[R with R1 with SttpClient]] =
      (self.toGraphQL <&> source.toGraphQL).flatMap { case (original, source) =>
        ZIO
          .fromOption(source.getSchemaBuilder.findStepWithFieldName(sourceFieldName))
          .mapBoth(
            _ =>
              ValidationError(
                s"Failed to extend field $targetFieldName because field $sourceFieldName was not found in the source.",
                s"The source schema must contain a root field named $sourceFieldName."
              ),
            { case (sourceStep, sourceField) =>
              original.transform(
                Transformer.Extend(
                  targetTypeName,
                  targetFieldName,
                  sourceField,
                  sourceStep,
                  (targetFields, field) => patchField(field, targetFields),
                  argumentMappings,
                  mapBatchResultToArguments
                )
              )
            }
          )
      }

    def patchField(field: Field, targetFields: Map[String, InputValue]): Field =
      if (field.isRoot) field.copy(fields = field.fields.map(patchField(_, Map.empty)))
      else if (field.name == targetFieldName)
        field.copy(
          name = sourceFieldName,
          arguments = field.arguments ++
            targetFields.flatMap { case (k, v) => argumentMappings.get(k).map(_(v)) }
        )
      else if (
        Types.innerType(field.fieldType).name.contains(targetTypeName) &&
        field.fields.exists(_.name == targetFieldName)
      )
        field.copy(fields =
          field.fields.filterNot(_.name == targetFieldName) ++
            (argumentMappings.keySet -- field.fields.map(_.name).toSet).map(Field(_, __Type(__TypeKind.NON_NULL), None))
        )
      else field
  }
}

object Source {

  trait SourceFromSchema[-R] extends Source[R] { self =>
    protected trait T

    def toGraphQL: Task[GraphQL[R]] =
      schema.map(implicit schema => _root_.caliban.graphQL(RootResolver(new T {})))

    def schema: Task[Schema[R, self.T]]
  }

  private case class GraphQLSource(url: String, headers: Map[String, String]) extends SourceFromSchema[SttpClient] {
    self =>
    def schema: Task[Schema[SttpClient, self.T]] =
      for {
        doc          <- SchemaLoader
                          .fromIntrospection(
                            url,
                            Some(headers.map { case (k, v) => Options.Header(k, v) }.toList),
                            supportIsRepeatable = false
                          )
                          .load
        remoteSchema <- ZIO
                          .succeed(RemoteSchema.parseRemoteSchema(doc))
                          .someOrFail(new Throwable("Failed to parse remote schema"))
      } yield new Schema[SttpClient, self.T] {
        override def toType(isInput: Boolean, isSubscription: Boolean): __Type = remoteSchema.queryType
        override def resolve(value: self.T): Step[SttpClient]                  =
          MetadataFunctionStep(field =>
            QueryStep(
              ZQuery
                .fromRequest(ProxyRequest(url, headers, field))(RemoteDataSource.dataSource)
                .map(responseValueToStep)
            )
          )
      }
  }

  private def responseValueToStep(responseValue: ResponseValue): Step[Any] =
    responseValue match {
      case ResponseValue.ListValue(values)   => Step.ListStep(values.map(responseValueToStep))
      case ResponseValue.ObjectValue(fields) =>
        val typeName = fields.toMap.get("__typename").collect { case StringValue(value) => value }.getOrElse("")
        Step.ObjectStep(typeName, fields.map { case (k, v) => k -> responseValueToStep(v) }.toMap)
      case ResponseValue.StreamValue(stream) => Step.StreamStep(stream.map(responseValueToStep))
      case value: Value                      => PureStep(value)
    }

  def graphQL(url: String, headers: Map[String, String] = Map.empty): Source[SttpClient] = GraphQLSource(url, headers)
  def rest(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def grpc(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def caliban[R](graphQL: GraphQL[R]): Source[R]                                         = new Source[R] {
    override def toGraphQL: Task[GraphQL[R]] =
      ZIO.succeed(graphQL) // TODO wrap with MetadataFunctionStep and inject field
  }
}
