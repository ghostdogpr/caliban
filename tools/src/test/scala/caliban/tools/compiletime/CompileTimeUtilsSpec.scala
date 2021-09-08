package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object CompileTimeUtilsSpec extends DefaultRunnableSpec {

  private val calibanCommonSettingsEquivalenceSpec = {
    def backAndForth(settings: CalibanCommonSettings): CalibanCommonSettings =
      CompileTimeUtils.calibanCommonSettingsEquivalence.from(
        CompileTimeUtils.calibanCommonSettingsEquivalence.to(settings)
      )

    val default =
      CalibanCommonSettings(
        clientName = Some("Client"),
        scalafmtPath = None,
        headers = Seq("header 1" -> "header value 1", "header 2" -> "header value 2"),
        packageName = Some("codegen.example"),
        genView = None,
        scalarMappings = Seq("mapping 1" -> "mapping value 1", "mapping 2" -> "mapping value 2"),
        imports = Seq("import 1", "import 2"),
        splitFiles = None,
        enableFmt = Some(false),
        extensibleEnums = Some(true)
      )

    suite("Equivalence[CalibanCommonSettings, List[String]]")(
      test("a <=> b <=> a") {
        assert(default)(equalTo(backAndForth(default)))
      },
      test("empty") {
        val settings = CalibanCommonSettings.empty
        assert(settings)(equalTo(backAndForth(settings)))
      },
      test("with empty lists") {
        val settings = default.copy(headers = Seq.empty, scalarMappings = Seq.empty, imports = Seq.empty)
        assert(settings)(equalTo(backAndForth(settings)))
      },
      test("with some empty lists") {
        val settings = default.copy(headers = Seq.empty, imports = Seq.empty)
        assert(settings)(equalTo(backAndForth(settings)))
      }
    )
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("CompileTimeUtils")(
      calibanCommonSettingsEquivalenceSpec
    )
}
