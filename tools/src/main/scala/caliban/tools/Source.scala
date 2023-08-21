package caliban.tools

import caliban.CalibanError.ValidationError
import caliban._
import caliban.execution.Field
import caliban.introspection.adt.__Type
import caliban.schema.{ ProxyRequest, Schema, Step }
import caliban.tools.stitching.RemoteQuery.QueryRenderer
import caliban.tools.stitching.RemoteResolver
import caliban.transformers.Transformer
import zio.query.DataSource
import zio.{ Chunk, Task, ZIO }

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
    filterBatchedValues: (ResponseValue, Field) => ResponseValue = (v, _) => v
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
            batchDataSource(step.dataSource)
          )
        case other                    => other
      }

    def batchDataSource(dataSource: DataSource[R1, ProxyRequest]): DataSource[R1, ProxyRequest] =
      DataSource.fromFunctionBatchedZIO(s"${dataSource.identifier}Batched") { requests =>
        val requestsMap     = requests.groupBy(_.url).flatMap { case (url, requests) =>
          requests
            .groupBy(request => QueryRenderer.renderField(request.field, ignoreArguments = true))
            .toList
            .flatMap { case (_, requests) =>
              requests.headOption match {
                case Some(head) =>
                  val batchedRequest = requests.map(_.field).tail.foldLeft(Option(head.field)) {
                    case (None, _)      => None
                    case (Some(f1), f2) => combineFieldArguments(f1, f2)
                  }
                  requests.map(request =>
                    (
                      request,
                      (
                        ProxyRequest(
                          url,
                          head.headers,
                          batchedRequest.getOrElse(request.field)
                        ),
                        request.field
                      )
                    )
                  )
                case None       => Chunk.empty
              }
            }
        }
        val batchedRequests = Chunk.fromIterable(requestsMap.values.map(_._1)).distinct

        dataSource
          .runAll(Chunk.single(batchedRequests))
          .map(results =>
            requests
              .flatMap(requestsMap.get)
              .flatMap { case (req, field) => results.lookup(req).map(_ -> field) }
              .collect { case (Right(value), field) => filterBatchedValues(value, field) }
          )
      }
  }

  private def combineFieldArguments(f1: Field, f2: Field): Option[Field] =
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
              case _                                                        => None
            }
          case _                    => None
        }
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
        schema        = new Schema[SttpClient, self.T] {
                          override def toType(isInput: Boolean, isSubscription: Boolean): __Type =
                            remoteSchema.queryType

                          override def resolve(value: self.T): Step[SttpClient] =
                            Step.ProxyStep(ProxyRequest(url, headers, _), dataSource)
                        }
      } yield schema
  }

  private val dataSource: DataSource[SttpClient, ProxyRequest] =
    DataSource.fromFunctionZIO[SttpClient, Throwable, ProxyRequest, ResponseValue]("RemoteDataSource") { request =>
      val remoteResolver =
        if (request.field.parentType.isEmpty) RemoteResolver.fromUrl2(request.url, request.headers)
        else RemoteResolver.fromUrl(request.url, request.headers)

      remoteResolver.run(request.field) // TODO: error handling
    }

  def graphQL(url: String, headers: Map[String, String] = Map.empty): Source[SttpClient] = GraphQLSource(url, headers)
  def rest(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def grpc(url: String): Source[SttpClient]                                              = GraphQLSource(url, Map.empty)
  def caliban[R](graphQL: GraphQL[R]): Source[R]                                         = new Source[R] {
    override def toGraphQL: Task[GraphQL[R]] = ZIO.succeed(graphQL)
  }
}
