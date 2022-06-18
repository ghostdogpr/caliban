package caliban.tools.compiletime

import zio.test._

object UtilsSpec extends ZIOSpecDefault {

  private val toPathDirSpec =
    suite(".toPathDir")(
      test("returns a different path according to the `baseDir` passed") {
        val result_0 = Utils.toPathDir("/user/Jules/workspace/awesome-project", "io.example.awesome.project.generated")
        val result_1 = Utils.toPathDir(
          "/user/Jules/workspace/awesome-project/target/scala-2.13/src_managed/main",
          "io.example.awesome.project.generated"
        )

        assertTrue(
          result_0 == "/user/Jules/workspace/awesome-project/src/main/scala/io/example/awesome/project/generated"
        ) &&
        assertTrue(
          result_1 == "/user/Jules/workspace/awesome-project/target/scala-2.13/src_managed/main/io/example/awesome/project/generated"
        )
      }
    )

  override def spec =
    suite("Utils spec")(
      toPathDirSpec
    )
}
