package caliban.tools

import caliban.CalibanError.ExecutionError
import caliban._
import caliban.execution.Field
import caliban.parsing.adt.OperationType
import caliban.schema.ProxyRequest
import sttp.client3.jsoniter._
import sttp.client3.{ basicRequest, DeserializationException, HttpError, Identity, RequestT, UriContext }
import zio.query.DataSource
import zio.{ Chunk, ZIO }

object RemoteDataSource {
  val dataSource: DataSource[SttpClient, ProxyRequest] =
    DataSource.fromFunctionZIO[SttpClient, Throwable, ProxyRequest, ResponseValue]("RemoteDataSource") { request =>
      (for {
        res  <- ZIO.serviceWithZIO[SttpClient](_.send(makeSttpRequest(request)))
        body <- ZIO.fromEither(res.body)
      } yield body).mapError(e => CalibanError.ExecutionError(e.toString, innerThrowable = Some(e)))
    }

  def batchDataSource[R](
    dataSource: DataSource[R, ProxyRequest]
  )(filterBatchedValues: (ResponseValue, Field) => ResponseValue): DataSource[R, ProxyRequest] =
    DataSource.fromFunctionBatchedZIO(s"${dataSource.identifier}Batched") { requests =>
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
                requests.map(request =>
                  (request, (ProxyRequest(url, head.headers, batchedRequest.getOrElse(request.field)), request.field))
                )
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
      .body(request.field.toGraphQLRequest(OperationType.Query, withTypeName = true))
      .headers(request.headers)
      .response(asJson[GraphQLResponse[CalibanError]])
      .mapResponse(resp =>
        resp.fold(
          {
            case DeserializationException(body, error) =>
              Left(ExecutionError(s"${error.getMessage}: $body", innerThrowable = Some(error)))
            case HttpError(_, statusCode)              => Left(ExecutionError(s"HTTP Error: $statusCode"))
          },
          resp =>
            Right(resp.data match {
              case v @ ResponseValue.ObjectValue(fields) if !request.field.isRoot =>
                // if it was not a root field, return the inner response instead
                fields.headOption.map(_._2).getOrElse(v)
              case x                                                              => x
            })
        )
      )
}
