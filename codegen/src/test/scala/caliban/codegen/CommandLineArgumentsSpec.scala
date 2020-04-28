package caliban.codegen

import caliban.codegen.CommandLineArguments.Header
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

class CommandLineArgumentsSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("CommandLineArguments")(
      test("full arguments") {
        val input  = List("schema", "output", "--scalafmtPath", "fmtPath", "--headers", "header1:value1,header2:value2")
        val result = CommandLineArguments.fromArgs(input)
        assert(result)(
          equalTo(
            Some(
              CommandLineArguments(
                "schema",
                "output",
                Some("fmtPath"),
                Some(List(Header("header1", "value1"), Header("header2", "value2")))
              )
            )
          )
        )
      }
    )
}
