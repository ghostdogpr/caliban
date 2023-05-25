package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import caliban.tools.Codegen.GenType

trait Config {
  case class ClientGenerationSettings(
    packageName: String,
    clientName: String = "Client",
    scalafmtPath: Option[String] = None,
    genView: Boolean = false,
    scalarMappings: List[(String, String)] = List.empty,
    imports: List[String] = List.empty,
    splitFiles: Boolean = false,
    enableFmt: Boolean = true,
    extensibleEnums: Boolean = false,
    supportIsRepeatable: Boolean = true
  ) {
    private[caliban] def toCalibanCommonSettings: CalibanCommonSettings =
      CalibanCommonSettings(
        clientName = Some(clientName),
        scalafmtPath = scalafmtPath,
        headers = List.empty,
        packageName = Some(packageName),
        genView = Some(genView),
        scalarMappings = scalarMappings,
        imports = imports,
        splitFiles = Some(splitFiles),
        enableFmt = Some(enableFmt),
        extensibleEnums = Some(extensibleEnums),
        GenType.Client,
        effect = None,
        abstractEffectType = None,
        preserveInputNames = None,
        supportIsRepeatable = Some(supportIsRepeatable)
      )

    private[caliban] def asScalaCode: String = {
      import Utils._
      s"""
         |ClientGenerationSettings(
         |  packageName = "$packageName",
         |  clientName = "$clientName",
         |  scalafmtPath = ${scalafmtPath.fold("None")(v => s"""Some("$v")""")},
         |  genView = $genView,
         |  scalarMappings = ${toScalaCode(scalarMappings)},
         |  imports = ${toScalaCode(imports)(v => s""""$v"""")},
         |  splitFiles = $splitFiles,
         |  enableFmt = $enableFmt,
         |  extensibleEnums = $extensibleEnums,
         |  supportIsRepeatable = $supportIsRepeatable
         |)
      """.stripMargin.trim
    }
  }
  object ClientGenerationSettings {
    def default: ClientGenerationSettings = ClientGenerationSettings(packageName = "generated")
  }
}

object Config extends Config
