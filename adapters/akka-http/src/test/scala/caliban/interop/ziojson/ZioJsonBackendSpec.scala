package caliban.interop.ziojson

import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import caliban.GraphQLRequest
import caliban.Value.StringValue

object ZioJsonBackendSpec extends DefaultRunnableSpec {

  val backend = new ZioJsonBackend()

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ZioJsonBackend")(
      suite("parseHttpRequest")(
        test("return empty GraphQLRequest") {
          val result = backend.parseHttpRequest(None, None, None, None)

          assert(result)(equalTo(Right(GraphQLRequest())))
        },
        test("return full GraphQLRequest") {
          val query = "query"
          val op    = "name"
          val vars  = """{"someVariable":"someValue"}"""
          val exts  = """{"someExtension":"someValue"}"""

          val result = backend.parseHttpRequest(Some(query), Some(op), Some(vars), Some(exts))

          val expected = GraphQLRequest(
            Some(query),
            Some(op),
            Some(Map("someVariable" -> StringValue("someValue"))),
            Some(Map("someExtension" -> StringValue("someValue")))
          )
          assert(result)(equalTo(Right(expected)))
        },
        test("return error for badly formed 'variables'") {
          val vars = """{"someValue"}"""

          val result = backend.parseHttpRequest(None, None, Some(vars), None)

          assert(result)(isLeft)
        },
        test("return error for badly formed 'extensions'") {
          val exts = """{"someValue"}"""

          val result = backend.parseHttpRequest(None, None, None, Some(exts))

          assert(result)(isLeft)
        }
      )
    )
}
