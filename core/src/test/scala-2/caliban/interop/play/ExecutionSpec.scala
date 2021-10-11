package caliban.interop.play

import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ExecutionSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Play ExecutionSpec")(
      testM("Play Json scalar") {
        import caliban.interop.play.json._
        import play.api.libs.json._
        final case class Queries(test: JsValue)

        val interpreter = graphQL(RootResolver(Queries(Json.obj(("a", JsNumber(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":{"a":333}}"""))
      }
    )
}
