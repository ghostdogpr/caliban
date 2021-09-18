package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import caliban.tools.compiletime.Config._
import zio.test._
import zio.test.environment.TestEnvironment

object ConfigSpec extends DefaultRunnableSpec {

  private val fullExample: ClientGenerationSettings =
    ClientGenerationSettings(
      packageName = "io.example.generated",
      clientName = "CalibanClient",
      scalafmtPath = Some("a/b/c"),
      headers = List("header 1" -> "value 1", ("b", "d")),
      genView = true,
      scalarMappings = List(("mapping key 1", "mapping value 1"), "abc" -> "def"),
      imports = List("zio.test._", "caliban.tools.compiletime._"),
      splitFiles = true,
      enableFmt = false,
      extensibleEnums = true
    )

  private val toCalibanCommonSettingsSpec =
    suite("#toCalibanCommonSettings")(
      test("fullExample")(
        assertTrue(
          fullExample.toCalibanCommonSettings ==
            CalibanCommonSettings(
              clientName = Some("CalibanClient"),
              scalafmtPath = Some("a/b/c"),
              headers = List("header 1" -> "value 1", ("b", "d")),
              packageName = Some("io.example.generated"),
              genView = Some(true),
              scalarMappings = List(("mapping key 1", "mapping value 1"), "abc" -> "def"),
              imports = List("zio.test._", "caliban.tools.compiletime._"),
              splitFiles = Some(true),
              enableFmt = Some(false),
              extensibleEnums = Some(true)
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
               |  headers = List.empty,
               |  genView = false,
               |  scalarMappings = List.empty,
               |  imports = List.empty,
               |  splitFiles = false,
               |  enableFmt = true,
               |  extensibleEnums = false
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
               |  headers = List(("header 1","value 1"),("b","d")),
               |  genView = true,
               |  scalarMappings = List(("mapping key 1","mapping value 1"),("abc","def")),
               |  imports = List("zio.test._","caliban.tools.compiletime._"),
               |  splitFiles = true,
               |  enableFmt = false,
               |  extensibleEnums = true
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
              headers = List.empty,
              genView = false,
              scalarMappings = List.empty,
              imports = List.empty,
              splitFiles = false,
              enableFmt = true,
              extensibleEnums = false
            )
        )
      )
    )

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ConfigSpec")(
      suite("ClientGenerationSettings")(
        toCalibanCommonSettingsSpec,
        asScalaCodeSpec,
        defaultSpec
      )
    )
}
