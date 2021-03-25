package caliban.tools

import caliban.tools.Options.Header
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object OptionsSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("OptionsSpec")(
      test("full arguments") {
        val input  = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "header1:value1,header2:value2")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("header1", "value1"), Header("header2", "value2"))),
                None,
                None,
                None,
                None,
                None
              )
            )
          )
        )
      },
      test("full arguments (--headers option first)") {
        val input  = List("schema", "output", "--headers", "header1:value1,header2:value2", "--scalafmtPath", "fmtPath")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("header1", "value1"), Header("header2", "value2"))),
                None,
                None,
                None,
                None,
                None
              )
            )
          )
        )
      },
      test("minimum arguments") {
        val input  = List("schema", "output")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options("schema", "output", None, None, None, None, None, None, None)
            )
          )
        )
      },
      test("not enough arguments") {
        val input  = List("schema")
        val result = Options.fromArgs(input)
        assert(result)(equalTo(None))
      },
      test("--scalafmtPath value missing") {
        val input  = List("schema", "output", "--scalafmtPath", "--headers", "header1:value1,header2:value2")
        val result = Options.fromArgs(input)
        assert(result)(equalTo(None))
      },
      test("empty list") {
        val result = Options.fromArgs(Nil)
        assert(result)(equalTo(None))
      },
      test("provide package name") {
        val input  = List("schema", "output", "--packageName", "com.github.ghostdogpr")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                None,
                None,
                Some("com.github.ghostdogpr"),
                None,
                None,
                None,
                None
              )
            )
          )
        )
      },
      test("provide effect") {
        val input  = List("schema", "output", "--effect", "cats.effect.IO")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                Some("cats.effect.IO"),
                None,
                None
              )
            )
          )
        )
      },
      test("provide genView") {
        val input  = List("schema", "output", "--genView", "true")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                Some(true),
                None,
                None,
                None
              )
            )
          )
        )
      },
      test("provide scalarMappings") {
        val input  = List("schema", "output", "--scalarMappings", "Long:scala.Long")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                Some(Map("Long" -> "scala.Long")),
                None
              )
            )
          )
        )
      },
      test("provide imports") {
        val input  = List("schema", "output", "--imports", "a.b.Clazz,b.c._")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                None,
                Some(List("a.b.Clazz", "b.c._"))
              )
            )
          )
        )
      }
    )
}
