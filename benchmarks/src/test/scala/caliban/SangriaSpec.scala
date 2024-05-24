package caliban

import caliban.SimpleQueryBenchmark.simpleQuery
import caliban.ComplexQueryBenchmark.fullIntrospectionQuery
import caliban.FragmentsQueryBenchmark.fragmentsQuery
import sangria.execution.Executor
import sangria.parser.QueryParser
import zio.test._

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContextExecutor, Future }

object SangriaSpec extends ZIOSpecDefault {
  implicit val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  override def spec =
    suite("Sangria")(
      test("Simple") {
        val future =
          Future.fromTry(QueryParser.parse(simpleQuery)).flatMap(queryAst => Executor.execute(Sangria.schema, queryAst))
        assertTrue(!Await.result(future, 1.minute).toString.contains("errors"))
      },
      test("Complex") {
        val future =
          Future
            .fromTry(QueryParser.parse(fullIntrospectionQuery))
            .flatMap(queryAst => Executor.execute(Sangria.schema, queryAst))
        assertTrue(!Await.result(future, 1.minute).toString.contains("errors"))
      },
      test("Fragments") {
        val future =
          Future
            .fromTry(QueryParser.parse(fragmentsQuery))
            .flatMap(queryAst => Executor.execute(Sangria.schema, queryAst))
        assertTrue(!Await.result(future, 1.minute).toString.contains("errors"))
      },
      test("Parser") {
        val future = Future.fromTry(QueryParser.parse(fullIntrospectionQuery))
        assertTrue(Await.result(future, 1.minute).definitions.nonEmpty)
      }
    )
}
