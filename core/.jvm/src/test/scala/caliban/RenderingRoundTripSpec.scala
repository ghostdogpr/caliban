package caliban

import caliban.parsing.Parser
import caliban.rendering.DocumentRenderer
import zio.ZIO
import zio.test._

import scala.io.Source

object RenderingRoundTripSpec extends ZIOSpecDefault {
  override def spec = suite("RenderingSpec - round-trip")(
    test("kitchen sink")(roundTrip("document-tests/kitchen-sink.graphql")),
    test("kitchen sink with query")(roundTrip("document-tests/kitchen-sink-query.graphql")),
    test("compact query")(roundTrip("document-tests/query-compact.graphql", isCompact = true)),
    test("compact kitchen sink")(roundTrip("document-tests/kitchen-sink-compact.graphql", isCompact = true)),
    test("extend tests")(roundTrip("document-tests/extend-tests.graphql"))
  )

  private def roundTrip(file: String, isCompact: Boolean = false) =
    ZIO.scoped(for {
      input   <- ZIO.fromAutoCloseable(ZIO.attempt(Source.fromResource(file))).map(_.mkString)
      doc     <- ZIO.fromEither(Parser.parseQuery(input))
      rendered = if (isCompact) DocumentRenderer.renderCompact(doc) else DocumentRenderer.render(doc)
      reparsed = Parser.parseQuery(rendered)
    } yield assertTrue(input == rendered, reparsed.isRight))
}
