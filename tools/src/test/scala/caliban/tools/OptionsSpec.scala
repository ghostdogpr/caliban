package caliban.tools

import caliban.tools.Options.Header
import zio.config.ReadError
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
            Right(
              Options(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("header1", "value1"), Header("header2", "value2"))),
                None,
                None,
                None,
                None,
                None,
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
            Right(
              Options(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("header1", "value1"), Header("header2", "value2"))),
                None,
                None,
                None,
                None,
                None,
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
            Right(
              Options("schema", "output", None, None, None, None, None, None, None, None, None, None, None, None)
            )
          )
        )
      },
      test("not enough arguments") {
        val input  = List("schema")
        val result = Options.fromArgs(input)
        assert(result)(isLeft(isSubtype[ReadError[Any]](anything)))
      },
      test("--scalafmtPath value missing") {
        val input  = List("schema", "output", "--scalafmtPath", "--headers", "header1:value1,header2:value2")
        val result = Options.fromArgs(input)
        assert(result)(isLeft(isSubtype[ReadError[Any]](anything)))
      },
      test("empty list") {
        val result = Options.fromArgs(Nil)
        assert(result)(isLeft(isSubtype[ReadError[Any]](anything)))
      },
      test("provide package name") {
        val input  = List("schema", "output", "--packageName", "com.github.ghostdogpr")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                Some("com.github.ghostdogpr"),
                None,
                None,
                None,
                None,
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
      test("provide client name") {
        val input  = List("schema", "output", "--clientName", "GraphqlClient.scala")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                Some("GraphqlClient.scala"),
                None,
                None,
                None,
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
      test("provide effect") {
        val input  = List("schema", "output", "--effect", "cats.effect.IO")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                Some("cats.effect.IO"),
                None,
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
      test("provide genView") {
        val input  = List("schema", "output", "--genView", "true")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                Some(true),
                None,
                None,
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
      test("provide extensibleEnums") {
        val input  = List("schema", "output", "--extensibleEnums", "true")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(true)
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
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                None,
                Some(Map("Long" -> "scala.Long")),
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
      test("provide imports") {
        val input  = List("schema", "output", "--imports", "a.b.Clazz,b.c._")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                Some(List("a.b.Clazz", "b.c._")),
                None,
                None,
                None,
                None
              )
            )
          )
        )
      },
      test("provide abstractEffectType") {
        val input  = List("schema", "output", "--effect", "F", "--abstractEffectType", "true")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                None,
                None,
                None,
                None,
                None,
                Some("F"),
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
      test("header with a colon in the value") {
        val input  = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "aaa:bbb:ccc")
        val result = Options.fromArgs(input)
        assert(result)(
          equalTo(
            Right(
              Options(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("aaa", "bbb:ccc"))),
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None
              )
            )
          )
        )
      }
    )
}
