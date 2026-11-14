package caliban.interop.tapir

import caliban._
import caliban.GraphQLResponseContext.ServerFailure
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.interop.tapir.TapirAdapterSpec.FakeServerRequest
import caliban.schema.Schema.auto._
import caliban.Value.{ EnumValue, IntValue, StringValue }
import sttp.model.{ Header, Method, StatusCode, Uri }
import sttp.tapir.DecodeResult
import zio.{ Trace, UIO, ZIO }
import zio.stream.ZStream
import zio.test._

object HttpInterpreterSpec extends ZIOSpecDefault {
  case class Query(value: String)

  private def interpreterOf[E](response: UIO[GraphQLResponse[E]]) = new GraphQLInterpreter[Any, E] {
    def check(query: String)(implicit trace: Trace)                    = ZIO.unit
    def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = response
  }

  private def post(headers: Header*) =
    FakeServerRequest(Method.POST, Uri.unsafeParse("http://localhost/graphql"), headers.toList)

  override def spec = suite("HttpInterpreterSpec")(
    test("GET query-param encoding round-trips object and list variables and the extensions object") {
      val variables  = Map[String, InputValue](
        "filter" -> ObjectValue(Map("name" -> StringValue("bob"))),
        "ids"    -> ListValue(List(StringValue("a"), StringValue("b")))
      )
      val extensions = Map[String, InputValue](
        "persistedQuery" -> ObjectValue(Map("version" -> IntValue(1), "sha256Hash" -> StringValue("abc")))
      )
      val request    = GraphQLRequest(query = Some("{ x }"), variables = Some(variables), extensions = Some(extensions))

      HttpInterpreter.queryFromQueryParams(HttpInterpreter.queryToQueryParams(request)) match {
        case DecodeResult.Value(decoded) =>
          assertTrue(
            decoded.query.contains("{ x }"),
            decoded.variables.contains(variables),
            decoded.extensions.contains(extensions)
          )
        case other                       =>
          assertTrue(false).label(s"expected a successful decode but got $other")
      }
    },
    test("GET query-param encoding of an enum variable decodes successfully") {
      val request = GraphQLRequest(query = Some("{ x }"), variables = Some(Map("status" -> EnumValue("ACTIVE"))))
      HttpInterpreter.queryFromQueryParams(HttpInterpreter.queryToQueryParams(request)) match {
        case DecodeResult.Value(_) => assertCompletes
        case other                 => assertTrue(false).label(s"expected a successful decode but got $other")
      }
    },
    test("scopes incoming request headers around HTTP and upload interpreter execution") {
      import StreamConstructor.zioStreams
      val interpreter = interpreterOf(
        IncomingRequestHeaders.get.map(headers => GraphQLResponse(Value.StringValue(headers.toString), Nil))
      )
      val request     = GraphQLRequest(query = Some("{ value }"))
      val server      = post(Header("Authorization", "Bearer token"))

      for {
        http   <- HttpInterpreter(interpreter).executeRequest[ZStream[Any, Throwable, Byte]](request, server)
        upload <- HttpUploadInterpreter(interpreter).executeRequest[ZStream[Any, Throwable, Byte]](request, server)
      } yield assertTrue(
        List(http, upload).forall(_._4.left.toOption.exists(_.toString.contains("List((Authorization,Bearer token))")))
      )
    },
    test("takes the status from the execution outcome and the status code config, not from mapError") {
      import StreamConstructor.zioStreams
      val gql     = "application/graphql-response+json"
      val json    = "application/json"
      val partial = interpreterOf(
        ZIO.succeed(
          GraphQLResponse[CalibanError](
            ResponseValue.ObjectValue(List("value" -> StringValue("v"))),
            List(CalibanError.ExecutionError("failed"))
          )
        )
      )
      def status[E](
        interpreter: GraphQLInterpreter[Any, E],
        config: StatusCodeConfig = StatusCodeConfig.default,
        accept: String = gql
      ) =
        HttpInterpreter(interpreter, config)
          .executeRequest[ZStream[Any, Throwable, Byte]](
            GraphQLRequest(query = Some("{ missing }")),
            post(Header("Accept", accept))
          )
          .map(response => (response._2.code, response._1.toString))

      for {
        api            <- graphQL(RootResolver(Query("value"))).interpreter
        rewritten      <- status(api.mapError(e => CalibanError.ExecutionError(e.getMessage)))
        unavailable    <- status(
                            interpreterOf(
                              GraphQLResponseContext
                                .markServerError(ServerFailure.Unavailable)
                                .as(GraphQLResponse(Value.NullValue, Nil))
                            )
                          )
        partialDefault <- status(partial)
        partialSpec    <- status(partial, StatusCodeConfig.spec)
        jsonDefault    <- status(api, accept = json)
        jsonSpec       <- status(api, StatusCodeConfig.spec, json)
      } yield assertTrue(
        rewritten == ((422, gql)),
        unavailable._1 == 503,
        partialDefault == ((200, gql)),
        partialSpec == ((294, gql)),
        jsonDefault == ((200, json)),
        jsonSpec == ((422, gql))
      )
    },
    test("materializes incoming headers only when requested and only once") {
      var evaluations = 0
      def headers     = {
        evaluations += 1
        List("x-test" -> "value")
      }

      for {
        unread <- IncomingRequestHeaders.locally(headers)(ZIO.succeed(evaluations))
        read   <- IncomingRequestHeaders.locally(headers)(IncomingRequestHeaders.get.zip(IncomingRequestHeaders.get))
      } yield assertTrue(
        unread == 0,
        read == ((List("x-test" -> "value"), List("x-test" -> "value"))),
        evaluations == 1
      )
    }
  )
}
