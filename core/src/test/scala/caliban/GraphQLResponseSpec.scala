package caliban

import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import zio.test._

object GraphQLResponseSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Nothing] =
    suite("GraphQLResponseSpec")(
      test("withExtension replaces an existing extension with the same key") {
        val response = GraphQLResponse(Value.NullValue, Nil)
          .withExtension("tracing", StringValue("v1"))
          .withExtension("cache", StringValue("cache"))
          .withExtension("tracing", StringValue("v2"))

        assertTrue(
          response.extensions == Some(
            ObjectValue(
              List(
                "tracing" -> StringValue("v2"),
                "cache"   -> StringValue("cache")
              )
            )
          )
        )
      }
    )
}
