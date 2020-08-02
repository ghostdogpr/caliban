package caliban

import caliban.Value.NullValue
import io.circe.Decoder.Result
import io.circe.Json
import io.circe.syntax._
import io.circe.parser._
import io.finch._
import shapeless._
import zio.interop.catz._
import zio.{ Runtime, Task, URIO }

object FinchAdapter extends Endpoint.Module[Task] {

  private def getGraphQLRequest(
    query: Option[String],
    op: Option[String],
    vars: Option[String],
    exts: Option[String]
  ): Result[GraphQLRequest] = {
    val variablesJs  = vars.flatMap(parse(_).toOption)
    val extensionsJs = exts.flatMap(parse(_).toOption)
    val fields = query.map(js => "query" -> Json.fromString(js)) ++
      op.map(o => "operationName"         -> Json.fromString(o)) ++
      variablesJs.map(js => "variables"   -> js) ++
      extensionsJs.map(js => "extensions" -> js)
    Json
      .fromFields(fields)
      .as[GraphQLRequest]
  }

  private def executeRequest[R, E](
    request: GraphQLRequest,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean
  )(implicit runtime: Runtime[R]) =
    runtime
      .unsafeRunToFuture(
        createRequest(request, interpreter, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
      )
      .future

  private def createRequest[R, E](
    request: GraphQLRequest,
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean,
    enableIntrospection: Boolean
  )(implicit runtime: Runtime[R]): URIO[R, Output[Json]] =
    interpreter
      .executeRequest(request, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
      .foldCause(cause => GraphQLResponse(NullValue, cause.defects).asJson, _.asJson)
      .map(gqlResult => Ok(gqlResult))

  private val queryParams =
    (paramOption[String]("query") ::
      paramOption[String]("operationName") ::
      paramOption[String]("variables") ::
      paramOption[String]("extensions")).mapAsync {
      case (query :: op :: vars :: exts :: HNil) =>
        Task.fromEither(getGraphQLRequest(query, op, vars, exts))
    }

  /**
   * Create a finch HTTP endpoint, provided you have an interpreter and a runtime
   * https://finagle.github.io/finch/
   *
   * @param interpreter the graphql interpreter
   * @param skipValidation skips the validation step if true
   * @param runtime the zio runtime used to execute the query
   * @tparam R the environment the `Runtime` requires
   * @tparam E the error type that the interpreter can fail with
   * @return a Finch endpoint in `Task` returning a Json response that is the result of executing incoming graphql
   *         queries against the interpreter
   */
  def makeHttpService[R, E](
    interpreter: GraphQLInterpreter[R, E],
    skipValidation: Boolean = false,
    enableIntrospection: Boolean = true
  )(implicit runtime: Runtime[R]): Endpoint[Task, Json :+: Json :+: CNil] =
    post(queryParams :: stringBodyOption :: header("content-type")) {
      (queryRequest: GraphQLRequest, body: Option[String], contentType: String) =>
        val request: GraphQLRequest = (queryRequest, body, contentType) match {
          case (_, Some(bodyValue), "application/json") =>
            parse(bodyValue).flatMap(_.as[GraphQLRequest]).getOrElse(GraphQLRequest())
          case (_, Some(_), "application/graphql") =>
            GraphQLRequest(body)
          case (queryRequest, _, _) if queryRequest.query.isDefined =>
            queryRequest
          // treat unmatched content-type as same as None of body.
          case _ =>
            GraphQLRequest()
        }
        executeRequest(
          request,
          interpreter,
          skipValidation = skipValidation,
          enableIntrospection = enableIntrospection
        )
    } :+: get(queryParams) { request: GraphQLRequest =>
      executeRequest(request, interpreter, skipValidation = skipValidation, enableIntrospection = enableIntrospection)
    }
}
