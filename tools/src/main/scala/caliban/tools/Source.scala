package caliban.tools

import caliban.CalibanError.ValidationError
import caliban._
import caliban.introspection.adt.__Type
import caliban.schema.{ ProxyRequest, Schema, Step }
import caliban.transformers.Transformer
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
  ): Source[R with R1] = new Source[R with R1] {
    def toGraphQL: Task[GraphQL[R with R1]] =
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
                  argumentMappings.keySet,
                  makeResolver(sourceStep)
                )
              )
            }
          )
      }

    def makeResolver(sourceStep: Step[R1])(targetFields: Map[String, InputValue]): Step[R1] =
      sourceStep match {
        case step: Step.ProxyStep[R1] =>
          Step.ProxyStep[R1](
            field =>
              step.makeRequest {
                // if this is the field we are extending, we need to replace its name and arguments
                if (field.name == targetFieldName)
                  field.copy(
                    name = sourceFieldName,
                    arguments = targetFields.flatMap { case (k, v) => argumentMappings.get(k).map(_(v)) }
                  )
                else field
              },
            RemoteDataSource.batchDataSource(step.dataSource)(argumentMappings, mapBatchResultToArguments)
          )
        case other                    => other
      }
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
          Step.ProxyStep(ProxyRequest(url, headers, _), RemoteDataSource.dataSource)
      }
  }

  def graphQL(url: String, headers: Map[String, String] = Map.empty): Source[SttpClient] = GraphQLSource(url, headers)
  def rest(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def grpc(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def caliban[R](graphQL: GraphQL[R]): Source[R]                                         = new Source[R] {
    override def toGraphQL: Task[GraphQL[R]] = ZIO.succeed(graphQL)
  }
}
