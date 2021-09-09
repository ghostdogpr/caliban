package caliban.tools.compiletime

import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object ConfigSpec extends DefaultRunnableSpec {
  import Config._

  private val equivalenceSpec = {
    def backAndForth(settings: GenerateClientSettings): GenerateClientSettings =
      GenerateClientSettings.equivalence.from(GenerateClientSettings.equivalence.to(settings))

    val example =
      GenerateClientSettings(
        clientName = "TestClient",
        packageName = "codegen.example",
        scalafmtPath = None,
        headers = List("header 1" -> "header value 1", "header 2" -> "header value 2"),
        genView = true,
        scalarMappings = List("mapping 1" -> "mapping value 1", "mapping 2" -> "mapping value 2"),
        imports = List("import 1", "import 2"),
        splitFiles = true,
        enableFmt = false,
        extensibleEnums = true
      )

    suite(".equivalence")(
      test("a <=> b <=> a") {
        assert(example)(equalTo(backAndForth(example)))
      },
      test("with empty lists") {
        val settings = example.copy(headers = List.empty, scalarMappings = List.empty, imports = List.empty)
        assert(settings)(equalTo(backAndForth(settings)))
      },
      test("with some empty lists") {
        val settings = example.copy(headers = List.empty, imports = List.empty)
        assert(settings)(equalTo(backAndForth(settings)))
      }
    )
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Config")(
      suite("GenerateClientSettings")(
        equivalenceSpec
      )
    )
}
