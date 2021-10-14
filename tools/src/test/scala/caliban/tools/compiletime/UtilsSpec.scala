package caliban.tools.compiletime

import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object UtilsSpec extends DefaultRunnableSpec {

  private val toPathDirSpec =
    suite(".toPathDir")(
      test("returns a different path according to the `baseDir` passed") {
        val result_0 = Utils.toPathDir("/user/Jules/workspace/awesome-project", "io.example.awesome.project.generated")
        val result_1 = Utils.toPathDir(
          "/user/Jules/workspace/awesome-project/target/scala-2.13/src_managed/main",
          "io.example.awesome.project.generated"
        )

        assert(result_0)(
          equalTo(
            "/user/Jules/workspace/awesome-project/src/main/scala/io/example/awesome/project/generated"
          )
        ) &&
        assert(result_1)(
          equalTo(
            "/user/Jules/workspace/awesome-project/target/scala-2.13/src_managed/main/io/example/awesome/project/generated"
          )
        )
      }
    )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Utils spec")(
      toPathDirSpec
    )
}
