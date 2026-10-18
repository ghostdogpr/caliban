package caliban.tracing

import caliban.InputValue.ListValue
import caliban.Value.{ BooleanValue, EnumValue, IntValue, StringValue }
import caliban.parsing.Parser
import caliban.{ InputValue, TestUtils }
import io.opentelemetry.api.common.AttributeKey
import zio._
import zio.test._

object SchemaTracerSpec extends ZIOSpecDefault {
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
      for {
        traced   <- (TestUtils.nestedApi @@ SchemaTracer.wrapper).interpreter
        response <- traced.execute(TestUtils.nestedSpreads(30))
        spans    <- TracingMock.getFinishedSpans
        documents = spans.flatMap(span => Option(span.getAttributes.get(AttributeKey.stringKey("document"))))
        plain    <- TestUtils.nestedApi.interpreter
        replayed <- ZIO.foreach(documents)(plain.execute(_))
      } yield assertTrue(
        documents.map(_.length < 20000) == List(true),
        replayed.map(_.data) == List(response.data)
      )
    }.provide(TracingMock.layer) @@ TestAspect.timeout(30.seconds)
  )
}
