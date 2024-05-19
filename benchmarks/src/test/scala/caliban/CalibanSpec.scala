package caliban

import caliban.SimpleQueryBenchmark.simpleQuery
import caliban.ComplexQueryBenchmark.fullIntrospectionQuery
import caliban.FragmentsQueryBenchmark.fragmentsQuery
import caliban.parsing.Parser
import zio.test._

object CalibanSpec extends ZIOSpecDefault {
  override def spec =
    suite("Caliban")(
      test("Simple") {
        val io = Caliban.interpreter.execute(simpleQuery)
        assertTrue(Caliban.run(io).errors.isEmpty)
      },
      test("Complex") {
        val io = Caliban.interpreter.execute(fullIntrospectionQuery)
        assertTrue(Caliban.run(io).errors.isEmpty)
      },
      test("Fragments") {
        val io = Caliban.interpreter.execute(fragmentsQuery)
        assertTrue(Caliban.run(io).errors.isEmpty)
      },
      test("Parser") {
        val io = Parser.parseQuery(fullIntrospectionQuery)
        assertTrue(Caliban.run(io).definitions.nonEmpty)
      }
    )
}
