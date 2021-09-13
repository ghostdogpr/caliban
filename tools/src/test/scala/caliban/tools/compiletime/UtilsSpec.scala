package caliban.tools.compiletime

import zio.test._
import zio.test.environment.TestEnvironment

object UtilsSpec extends DefaultRunnableSpec {

  private val toPathDirSpec =
    suite(".toPathDir")(
      test("returns a valid path") {
        val result = Utils.toPathDir("/user/Jules/workspace/awesome-project", "io.example.awesome.project.generated")

        assertTrue(
          result == "/user/Jules/workspace/awesome-project/src/main/scala/io/example/awesome/project/generated"
        )
      }
    )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Utils spec")(
      toPathDirSpec
    )
}
