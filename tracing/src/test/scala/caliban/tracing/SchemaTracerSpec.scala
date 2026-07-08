package caliban.tracing

import caliban.InputValue
import caliban.InputValue.ListValue
import caliban.Value.{ BooleanValue, EnumValue, IntValue, StringValue }
import caliban.parsing.Parser
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
    }
  )
}
