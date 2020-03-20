package caliban.execution

import caliban.GraphQL._
import caliban.Macros.gqldoc
import caliban.RootResolver
import spray.json.{ JsNumber, JsObject, JsValue }
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment


object JVMExecutionSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("JVMExecutionSpec")(
      testM("Spray json scalar") {
        import caliban.interop.spray.json._
        case class Queries(test: JsValue)

        val interpreter = graphQL(RootResolver(Queries(JsObject(("a", JsNumber(333)))))).interpreter
        val query       = gqldoc("""
             {
               test
             }""")

        assertM(interpreter.flatMap(_.execute(query)).map(_.data.toString))(equalTo("""{"test":{"a":333}}"""))
      }
    )
}
