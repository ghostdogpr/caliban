package caliban.codegen

import caliban.tools.Options
import caliban.tools.Options.Header
import zio.test.*

object OptionsParserSpec extends ZIOSpecDefault {
  override def spec =
    suite("OptionsParserSpec")(
      test("full arguments") {
        val input = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "header1:value1,header2:value2")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result == Some(
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
                None,
                None,
                None
              )
            )
          )
        }
      },
      test("full arguments (--headers option first)") {
        val input = List("schema", "output", "--headers", "header1:value1,header2:value2", "--scalafmtPath", "fmtPath")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result == Some(
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
                None,
                None,
                None
              )
            )
          )
        }
      },
      test("minimum arguments") {
        val input = List("schema", "output")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
      },
      test("not enough arguments") {
        val input = List("schema")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(result == None)
        }
      },
      test("--scalafmtPath value missing") {
        val input = List("schema", "output", "--scalafmtPath", "--headers", "header1:value1,header2:value2")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(result == None)
        }
      },
      test("empty list") {
        val input = Nil
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(result == None)
        }
      },
      test("provide package name") {
        val input = List("schema", "output", "--packageName", "com.github.ghostdogpr")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
      },
      test("provide client name") {
        val input = List("schema", "output", "--clientName", "GraphqlClient.scala")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
              Some(
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
                  None,
                  None,
                  None
                )
              )
          )
        }
      },
      test("provide effect") {
        val input = List("schema", "output", "--effect", "cats.effect.IO")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
              Some(
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
                  None,
                  None,
                  None
                )
              )
          )
        }
      },
      test("provide genView") {
        val input = List("schema", "output", "--genView", "true")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
              Some(
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
                  None,
                  None,
                  None
                )
              )
          )
        }
      },
      test("provide extensibleEnums") {
        val input = List("schema", "output", "--extensibleEnums", "true")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
                  None,
                  None,
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
        }
      },
      test("provide scalarMappings") {
        val input = List("schema", "output", "--scalarMappings", "Long:scala.Long")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
                  Some(Map("Long" -> "scala.Long")),
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
      },
      test("provide imports") {
        val input = List("schema", "output", "--imports", "a.b.Clazz,b.c._")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
                  None,
                  Some(List("a.b.Clazz", "b.c._")),
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
      },
      test("provide abstractEffectType") {
        val input = List("schema", "output", "--effect", "F", "--abstractEffectType", "true")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
              Some(
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
                  None,
                  None,
                  None
                )
              )
          )
        }
      },
      test("provide preserveInputNames") {
        val input = List("schema", "output", "--preserveInputNames", "true")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
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
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(true),
                  None,
                  None
                )
              )
          )
        }
      },
      test("header with a colon in the value") {
        val input = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "aaa:bbb:ccc")
        OptionsParser.fromArgs(input).map { result =>
          assertTrue(
            result ==
              Some(
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
                  None,
                  None,
                  None
                )
              )
          )
        }
      }
    )
}
