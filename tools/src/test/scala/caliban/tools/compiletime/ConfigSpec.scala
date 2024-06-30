package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import caliban.tools.Codegen.GenType
import caliban.tools.compiletime.Config._
import zio.test._

object ConfigSpec extends ZIOSpecDefault {

  private val fullExample: ClientGenerationSettings =
    ClientGenerationSettings(
      packageName = "io.example.generated",
      clientName = "CalibanClient",
      scalafmtPath = Some("a/b/c"),
      genView = true,
      scalarMappings = List(("mapping key 1", "mapping value 1"), "abc" -> "def"),
      imports = List("zio.test._", "caliban.tools.compiletime._"),
      splitFiles = true,
      enableFmt = false,
      extensibleEnums = true,
      supportIsRepeatable = true,
      excludeDeprecated = true,
      queriesPath = Some("c/b/a")
    )

  private val toCalibanCommonSettingsSpec =
    suite("#toCalibanCommonSettings")(
      test("fullExample")(
        assertTrue(
          fullExample.toCalibanCommonSettings ==
            CalibanCommonSettings(
              genType = GenType.Client,
              clientName = Some("CalibanClient"),
              scalafmtPath = Some("a/b/c"),
              headers = List.empty,
              packageName = Some("io.example.generated"),
              genView = Some(true),
              scalarMappings = List(("mapping key 1", "mapping value 1"), "abc" -> "def"),
              imports = List("zio.test._", "caliban.tools.compiletime._"),
              splitFiles = Some(true),
              enableFmt = Some(false),
              extensibleEnums = Some(true),
              effect = None,
              abstractEffectType = None,
              preserveInputNames = None,
              supportIsRepeatable = Some(true),
              addDerives = None,
              envForDerives = None,
              excludeDeprecated = Some(true),
              queriesPath = Some("c/b/a")
            )
        )
      )
    )

  private val asScalaCodeSpec =
    suite("#asScalaCode")(
      test("default")(
        assertTrue(
          ClientGenerationSettings.default.asScalaCode ==
            s"""
               |ClientGenerationSettings(
               |  packageName = "generated",
               |  clientName = "Client",
               |  scalafmtPath = None,
               |  genView = false,
               |  scalarMappings = List.empty,
               |  imports = List.empty,
               |  splitFiles = false,
               |  enableFmt = true,
               |  extensibleEnums = false,
               |  supportIsRepeatable = true,
               |  excludeDeprecated = false,
               |  queriesPath = None
               |)
            """.stripMargin.trim
        )
      ),
      test("full example")(
        assertTrue(
          fullExample.asScalaCode ==
            s"""
               |ClientGenerationSettings(
               |  packageName = "io.example.generated",
               |  clientName = "CalibanClient",
               |  scalafmtPath = Some("a/b/c"),
               |  genView = true,
               |  scalarMappings = List(("mapping key 1","mapping value 1"),("abc","def")),
               |  imports = List("zio.test._","caliban.tools.compiletime._"),
               |  splitFiles = true,
               |  enableFmt = false,
               |  extensibleEnums = true,
               |  supportIsRepeatable = true,
               |  excludeDeprecated = true,
               |  queriesPath = Some("c/b/a")
               |)
            """.stripMargin.trim
        )
      )
    )

  private val defaultSpec =
    suite(".default")(
      test("use default value")(
        assertTrue(
          ClientGenerationSettings.default ==
            ClientGenerationSettings(
              packageName = "generated",
              clientName = "Client",
              scalafmtPath = None,
              genView = false,
              scalarMappings = List.empty,
              imports = List.empty,
              splitFiles = false,
              enableFmt = true,
              extensibleEnums = false,
              excludeDeprecated = false
            )
        )
      )
    )

  override def spec =
    suite("Config spec")(
      suite("ClientGenerationSettings")(
        toCalibanCommonSettingsSpec,
        asScalaCodeSpec,
        defaultSpec
      )
    )
}
