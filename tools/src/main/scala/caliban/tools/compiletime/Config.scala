package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import zio.prelude.Equivalence

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

    private[caliban] def asArgs: List[String] = GenerateClientSettings.equivalence.to(this)
  }
  object GenerateClientSettings {
    def default: GenerateClientSettings = GenerateClientSettings(clientName = "Client", packageName = "generated")

    private[caliban] val equivalence: Equivalence[GenerateClientSettings, List[String]] = {
      import Utils.Equivalences._

      Equivalence[GenerateClientSettings, List[String]](
        to = s =>
          List(
            s.clientName,
            s.packageName,
            optionStringEquivalence.to(s.scalafmtPath),
            s.genView.toString,
            s.splitFiles.toString,
            s.enableFmt.toString,
            s.extensibleEnums.toString,
            s.headers.size.toString,
            s.scalarMappings.size.toString,
            s.imports.size.toString
          ) ++ s.headers.map(tupleStringEquivalence.to)
            ++ s.scalarMappings.map(tupleStringEquivalence.to)
            ++ s.imports.map(s => noSpaceStringEquivalence.from(s.trim)),
        from = (args: List[String]) => {
          val List(clientName, packageName)                         = args.slice(0, 2)
          val List(scalafmtPath)                                    = args.slice(2, 3).map(optionStringEquivalence.from)
          val List(genView, splitFiles, enableFmt, extensibleEnums) = args.slice(3, 7).map(_.toBoolean)
          val List(headersSize, scalarMappingsSize, importsSize)    = args.slice(7, 10).map(_.toInt)

          val headers: List[(String, String)] =
            if (headersSize > 0) args.slice(10, 10 + headersSize).map(tupleStringEquivalence.from)
            else List.empty

          val scalarMappings: List[(String, String)] =
            if (scalarMappingsSize > 0)
              args.slice(10 + headersSize, 10 + headersSize + scalarMappingsSize).map(tupleStringEquivalence.from)
            else List.empty

          val imports: List[String] =
            if (importsSize > 0)
              args
                .slice(10 + headersSize + scalarMappingsSize, 10 + headersSize + scalarMappingsSize + importsSize)
                .map(s => noSpaceStringEquivalence.to(NoSpaceString(s)))
            else List.empty

          GenerateClientSettings(
            clientName = clientName,
            packageName = packageName,
            scalafmtPath = scalafmtPath,
            headers = headers,
            genView = genView,
            scalarMappings = scalarMappings,
            imports = imports,
            splitFiles = splitFiles,
            enableFmt = enableFmt,
            extensibleEnums = extensibleEnums
          )
        }
      )
    }

    private[caliban] def fromArgs(args: Seq[String]): GenerateClientSettings = equivalence.from(args.toList)
  }
}

object Config extends Config
