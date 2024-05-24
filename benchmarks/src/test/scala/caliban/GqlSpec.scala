package caliban

import caliban.ComplexQueryBenchmark.fullIntrospectionQuery
import caliban.FragmentsQueryBenchmark.fragmentsQuery
import caliban.SimpleQueryBenchmark.simpleQuery
import cats.effect.IO
import zio.test._

object GqlSpec extends ZIOSpecDefault {
  override def spec =
    suite("Gql")(
      test("Simple") {
        val io = gql.Compiler[IO].compile(Gql.schema, simpleQuery) match {
          case Right(gql.Application.Query(run)) => run
          case _                                 => IO.raiseError(new Exception("Failed to compile"))
        }
        assertTrue(Gql.run(io).errors.isEmpty)
      },
      test("Complex") {
        val io = gql.Compiler[IO].compile(Gql.schema, fullIntrospectionQuery) match {
          case Right(gql.Application.Query(run)) => run
          case _                                 => IO.raiseError(new Exception("Failed to compile"))
        }
        assertTrue(Gql.run(io).errors.isEmpty)
      },
      test("Fragments") {
        val io = gql.Compiler[IO].compile(Gql.schema, fragmentsQuery) match {
          case Right(gql.Application.Query(run)) => run
          case _                                 => IO.raiseError(new Exception("Failed to compile"))
        }
        assertTrue(Gql.run(io).errors.isEmpty)
      },
      test("Parser") {
        val result = gql.parser.parseQuery(fullIntrospectionQuery)
        assertTrue(result.isRight)
      }
    )
}
