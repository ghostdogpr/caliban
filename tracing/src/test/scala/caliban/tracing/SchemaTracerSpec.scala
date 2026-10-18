package caliban.tracing

import caliban.InputValue.ListValue
import caliban.Value.{ BooleanValue, EnumValue, IntValue, StringValue }
import caliban.parsing.Parser
import caliban.schema.Schema
import caliban.schema.Schema.auto._
import caliban.{ graphQL, InputValue, RootResolver }
import io.opentelemetry.api.common.AttributeKey
import zio._
import zio.test._

object SchemaTracerSpec extends ZIOSpecDefault {
  case class Nested(value: Int, next: () => Nested)
  case class Query(root: Nested)
  lazy val nested: Nested                             = Nested(1, () => nested)
  implicit lazy val nestedSchema: Schema[Any, Nested] = Schema.gen

  override def spec = suite("SchemaTracerSpec")(
    test("maskArguments masks scalar, list, enum and boolean argument values") {
      val args   = Map[String, InputValue](
        "s"    -> StringValue("secret"),
        "list" -> ListValue(List(StringValue("ssn:123-45-6789"), StringValue("secret2"))),
        "enum" -> EnumValue("SECRET"),
        "bool" -> BooleanValue(true),
        "int"  -> IntValue(42)
      )
      val masked = SchemaTracer.maskArguments(args)
      assertTrue(
        masked("s") == StringValue(""),
        masked("list") == ListValue(List(StringValue(""), StringValue(""))),
        masked("enum") == EnumValue("__REDACTED"),
        masked("bool") == BooleanValue(false),
        masked("int") == IntValue(0),
        // the masked enum must render as a syntactically valid enum value, not a bare empty name
        Parser.parseInputValue(masked("enum").toInputString) == Right(EnumValue("__REDACTED"))
      )
    },
    test("renders the document of fragments spread inside fragments once per shared field") {
      val spreads   = (1 to 30).map(k => s"...A$k @include(if: true) ...A$k @skip(if: false) ...B$k")
      val fragments =
        (0 until 30).flatMap(k => List("A", "B").map(name => s"fragment $name$k on Nested { next { ${spreads(k)} } }"))
      val query     =
        "{ root { ...A0 ...B0 } } " + fragments.mkString(
          " "
        ) + " fragment A30 on Nested { value } fragment B30 on Nested { value }"
      val api       = graphQL(RootResolver(Query(nested)))

      for {
        traced   <- (api @@ SchemaTracer.wrapper).interpreter
        response <- traced.execute(query)
        spans    <- TracingMock.getFinishedSpans
        documents = spans.flatMap(span => Option(span.getAttributes.get(AttributeKey.stringKey("document"))))
        plain    <- api.interpreter
        replayed <- ZIO.foreach(documents)(plain.execute(_))
      } yield assertTrue(
        documents.map(_.length < 20000) == List(true),
        replayed.map(_.data) == List(response.data)
      )
    }.provide(TracingMock.layer) @@ TestAspect.timeout(30.seconds)
  )
}
