package caliban.schema

import caliban.CalibanError.ExecutionError
import caliban.Value.EnumValue
import zio.test.Assertion._
import zio.test._

object ArgBuilderJavaEnumSpec extends ZIOSpecDefault {
  def spec = suite("ArgBuilder Java enums")(
    test("should support Java enums") {
      assert(ArgBuilder.enumJava[Color].build(EnumValue("Red")))(
        isRight(equalTo(Color.Red))
      )
    },
    test("should fail for invalid enum value") {
      assert(ArgBuilder.enumJava[Color].build(EnumValue("Purple")))(
        isLeft(
          hasField[ExecutionError, String](
            "msg",
            _.msg,
            equalTo("'Purple' is not a valid value of Color. Valid values are: [Red, Green, Blue]")
          )
        )
      )
    }
  )
}
