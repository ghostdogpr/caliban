package caliban.gateway.audit

import com.github.plokhotnyuk.jsoniter_scala.core.readFromArray
import zio.test._

import java.nio.charset.StandardCharsets
import scala.util.Try

object MainSpec extends ZIOSpecDefault {

  def spec = suite("MainSpec")(
    test("decodes upstream subgraph descriptions without changing their order or values") {
      val json =
        """[
          |  {"name":"products","url":"http://127.0.0.1:4200/basic/products","sdl":"type Query { product: String }"},
          |  {"name":"reviews","url":"http://127.0.0.1:4200/basic/reviews","sdl":"extend type Product @key(fields: \"id\") { id: ID! }"}
          |]""".stripMargin

      val result = Try(readFromArray[List[Main.SubgraphInput]](json.getBytes(StandardCharsets.UTF_8))).toEither

      assertTrue(
        result == Right(
          List(
            Main.SubgraphInput(
              "products",
              "http://127.0.0.1:4200/basic/products",
              "type Query { product: String }"
            ),
            Main.SubgraphInput(
              "reviews",
              "http://127.0.0.1:4200/basic/reviews",
              "extend type Product @key(fields: \"id\") { id: ID! }"
            )
          )
        )
      )
    },
    test("rejects malformed upstream subgraph descriptions") {
      val missingSdl = """[{"name":"products","url":"http://127.0.0.1:4200/products"}]"""

      val result = Try(readFromArray[List[Main.SubgraphInput]](missingSdl.getBytes(StandardCharsets.UTF_8)))

      assertTrue(result.isFailure)
    }
  )
}
