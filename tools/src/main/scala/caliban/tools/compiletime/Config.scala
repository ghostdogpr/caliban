package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings

trait Config {
  case class GenerateClientSettings(
    clientName: String,
    packageName: String,
    scalafmtPath: Option[String] = None,
    headers: List[(String, String)] = List.empty,
    genView: Boolean = false,
    scalarMappings: List[(String, String)] = List.empty,
    imports: List[String] = List.empty,
    splitFiles: Boolean = false,
    enableFmt: Boolean = true,
    extensibleEnums: Boolean = false
  )                             {
    private[caliban] def toCalibanCommonSettings: CalibanCommonSettings =
      CalibanCommonSettings(
        clientName = Some(clientName),
        scalafmtPath = scalafmtPath,
        headers = headers,
        packageName = Some(packageName),
        genView = Some(genView),
        scalarMappings = scalarMappings,
        imports = imports,
        splitFiles = Some(splitFiles),
        enableFmt = Some(enableFmt),
        extensibleEnums = Some(extensibleEnums)
      )

    private[caliban] def asScalaCode: String = {
      import Utils._
      s"""
         |GenerateClientSettings(
         |  clientName = "$clientName",
         |  packageName = "$packageName",
         |  scalafmtPath = ${scalafmtPath.fold("None")(v => s"""Some("$v")""")},
         |  headers = ${toScalaCode(headers)},
         |  genView = $genView,
         |  scalarMappings = ${toScalaCode(scalarMappings)},
         |  imports = ${toScalaCode(imports)(v => s""""$v"""")},
         |  splitFiles = $splitFiles,
         |  enableFmt = $enableFmt,
         |  extensibleEnums = $extensibleEnums
         |)
      """.stripMargin.trim
    }
  }
  object GenerateClientSettings {
    def default: GenerateClientSettings = GenerateClientSettings(clientName = "Client", packageName = "generated")
  }
}

object Config extends Config
