package caliban.interop.tapir

import caliban.GraphQLRequest
import caliban.InputValue
import caliban.InputValue.{ ListValue, ObjectValue }
import caliban.Value.{ EnumValue, IntValue, StringValue }
import sttp.tapir.DecodeResult
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
    }
  )
}
