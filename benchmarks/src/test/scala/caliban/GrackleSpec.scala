package caliban

import caliban.ComplexQueryBenchmark.fullIntrospectionQuery
import caliban.FragmentsQueryBenchmark.fragmentsQuery
import caliban.SimpleQueryBenchmark.simpleQuery
import zio.test._

object GrackleSpec extends ZIOSpecDefault {
  override def spec =
    suite("Grackle")(
      test("Simple") {
        val io = Grackle.compileAndRun(simpleQuery)
        assertTrue(!Grackle.run(io).toString.contains("errors"))
      },
      test("Complex") {
        val io = Grackle.compileAndRun(fullIntrospectionQuery)
        assertTrue(!Grackle.run(io).toString.contains("errors"))
      },
      test("Fragments") {
        val io = Grackle.compileAndRun(fragmentsQuery)
        assertTrue(!Grackle.run(io).toString.contains("errors"))
      },
      test("Parser") {
        val result = Grackle.compiler.compile(fullIntrospectionQuery)
        assertTrue(result.toEither.isRight)
      }
    )
}
