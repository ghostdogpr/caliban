package caliban.tools

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban._
import caliban.execution.Field
import caliban.parsing.adt.OperationType
import sttp.client3.jsoniter._
import sttp.client3.{ basicRequest, DeserializationException, HttpError, Identity, RequestT, UriContext }
import zio.query.DataSource
import zio.{ Chunk, ZIO }

object RemoteDataSource {
  case class ProxyRequest(url: String, headers: Map[String, String], field: Field)
      extends zio.query.Request[Throwable, ResponseValue]

  val dataSource: DataSource[SttpClient, ProxyRequest] =
    DataSource.fromFunctionBatchedZIO[SttpClient, Throwable, ProxyRequest, ResponseValue]("RemoteDataSource") {
      requests =>
        val requestsMap     = requests.groupBy(_.url).flatMap { case (url, requests) =>
          requests
            .groupBy(_.field.copy(arguments = Map.empty).toSelection)
            .toList
            .flatMap { case (_, requests) =>
              requests.toList match {
                case Nil          => Chunk.empty
                case head :: tail =>
                  val batchedRequest = tail.map(_.field).foldLeft(Option(head.field)) {
                    case (None, _)      => None
                    case (Some(f1), f2) => combineFieldArguments(f1, f2)
                  }
                  requests
                    .map(request => (request, ProxyRequest(url, head.headers, batchedRequest.getOrElse(request.field))))
              }
            }
        }
        // TODO requests to the same url with different fields should be batched
        val batchedRequests = Chunk.fromIterable(requestsMap.values).distinct

        ZIO
          .foreachPar(batchedRequests)(request =>
            (for {
              res  <- ZIO.serviceWithZIO[SttpClient](_.send(makeSttpRequest(request)))
              body <- ZIO.fromEither(res.body)
            } yield request -> body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
          )
          .map(_.toMap)
          .map(results =>
            requests.flatMap(req =>
              requestsMap.get(req).flatMap(results.get).map {
                case value @ ObjectValue(fields) if !req.field.isRoot =>
                  fields.collectFirst { case (fieldName, value) if req.field.name == fieldName => value }
                    .getOrElse(value)
                case other                                            => other
              }
            )
          )
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

  private def makeSttpRequest(request: ProxyRequest): RequestT[Identity, Either[ExecutionError, ResponseValue], Any] =
    basicRequest
      .post(uri"${request.url}")
      .body(request.field.withTypeName.toGraphQLRequest(OperationType.Query))
      .headers(request.headers)
      .response(asJson[GraphQLResponse[CalibanError]])
      .mapResponse(_.map(_.data).left.map {
        case DeserializationException(body, error) =>
          ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error))
        case HttpError(_, statusCode)              => ExecutionError(s"HTTP Error: $statusCode")
      })
}
