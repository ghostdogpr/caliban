package caliban.rendering

import caliban.parsing.{ Parser, SourceMapper }
import caliban.parsing.adt.Document
import caliban.{ graphQL, RenderingSpecSchema }
import zio.test.{ assertTrue, ZIOSpecDefault }
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.Macros.gqldoc

object DocumentRenderingSpec extends ZIOSpecDefault {

  val spec = suite("Document Rendering")(
    test("render a schema document") {
      val api      = graphQL(RenderingSpecSchema.resolverSchema)
      val document = api.toDocument
      assertTrue(Renderer.render(document) == "bob")
    },
    test("render a query document") {
      val document = gqldoc("""query User($isTrue: Boolean!, $isFalse: String) {
                                user(id: 123) {
                                  ...UserFragment
                                  reviews(limit: 10) {
                                    id @skip(if: $isTrue)
                                  }
                                  ... on User {
                                    id
                                  }
                                }
                              }
                              
                              fragment UserFragment on User {
                                age
                                name { first, last }
                              }
                              """)

      for {
        parsed <- Parser.parseQuery(document)
      } yield assertTrue(Renderer.render(parsed) == "bob")
    }
  )

}
