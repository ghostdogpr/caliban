package caliban.rendering

import caliban.{ graphQL, RenderingSpecSchema }
import zio.test.{ assertTrue, ZIOSpecDefault }
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._

object DocumentRenderingSpec extends ZIOSpecDefault {

  val spec = suite("Document Rendering")(
    test("render a schema document") {
      val api      = graphQL(RenderingSpecSchema.resolverSchema)
      val document = api.toDocument
      assertTrue(Renderer.render(document) == "bob")
    }
  )

}
