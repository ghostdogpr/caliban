package caliban.interop.tapir

import caliban.{ GraphQLInterpreter, GraphQLRequest, GraphQLResponse, IncomingRequestHeaders, InputValue, Value }
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.interop.tapir.TapirAdapterSpec.FakeServerRequest
import caliban.Value.{ EnumValue, IntValue, StringValue }
import sttp.model.{ Header, Method, Uri }
import sttp.tapir.DecodeResult
import zio.{ Trace, ZIO }
import zio.stream.ZStream
import zio.test._

object HttpInterpreterSpec extends ZIOSpecDefault {
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
    test("scopes incoming request headers around HTTP interpreter execution") {
      import StreamConstructor.zioStreams
      val interpreter = new GraphQLInterpreter[Any, Nothing] {
        def check(query: String)(implicit trace: Trace)                    = ZIO.unit
        def executeRequest(request: GraphQLRequest)(implicit trace: Trace) =
          IncomingRequestHeaders.get.map(headers => GraphQLResponse(Value.StringValue(headers.toString), Nil))
      }
      val request     = FakeServerRequest(
        Method.POST,
        Uri.unsafeParse("http://localhost/graphql"),
        List(Header("Authorization", "Bearer token"))
      )

      HttpInterpreter(interpreter)
        .executeRequest[ZStream[Any, Throwable, Byte]](GraphQLRequest(query = Some("{ value }")), request)
        .map { case (_, _, _, body) =>
          assertTrue(body.left.toOption.exists(_.toString.contains("List((Authorization,Bearer token))")))
        }
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
        read == (List("x-test" -> "value"), List("x-test" -> "value")),
        evaluations == 1
      )
    }
  )
}
