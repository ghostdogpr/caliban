package caliban.tools

import caliban.tools.Options.Header
import zio.test.Assertion._
import zio.test._

object OptionsSpec extends ZIOSpecDefault {
  override def spec =
    suite("OptionsSpec")(
      test("full arguments") {
        val input = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "header1:value1,header2:value2")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("full arguments (--headers option first)") {
        val input = List("schema", "output", "--headers", "header1:value1,header2:value2", "--scalafmtPath", "fmtPath")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("minimum arguments") {
        val input = List("schema", "output")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("not enough arguments") {
        val input = List("schema")
        Options.fromArgs(input).exit.map(result => assert(result)(fails(anything)))
      },
      test("--scalafmtPath value missing") {
        val input = List("schema", "output", "--scalafmtPath", "--headers", "header1:value1,header2:value2")
        Options.fromArgs(input).exit.map(result => assert(result)(fails(anything)))
      },
      test("empty list") {
        Options.fromArgs(Nil).exit.map(result => assert(result)(fails(anything)))
      },
      test("provide package name") {
        val input = List("schema", "output", "--packageName", "com.github.ghostdogpr")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide client name") {
        val input = List("schema", "output", "--clientName", "GraphqlClient.scala")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide effect") {
        val input = List("schema", "output", "--effect", "cats.effect.IO")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide genView") {
        val input = List("schema", "output", "--genView", "true")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide extensibleEnums") {
        val input = List("schema", "output", "--extensibleEnums", "true")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  Some(true),
                  None
                )
            )
          )
      },
      test("provide scalarMappings") {
        val input = List("schema", "output", "--scalarMappings", "Long:scala.Long")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide imports") {
        val input = List("schema", "output", "--imports", "a.b.Clazz,b.c._")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide abstractEffectType") {
        val input = List("schema", "output", "--effect", "F", "--abstractEffectType", "true")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      },
      test("provide preserveInputNames") {
        val input = List("schema", "output", "--preserveInputNames", "true")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  Some(true)
                )
            )
          )
      },
      test("header with a colon in the value") {
        val input = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "aaa:bbb:ccc")
        Options
          .fromArgs(input)
          .map(result =>
            assertTrue(
              result ==
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
                  None,
                  None
                )
            )
          )
      }
    )
}
