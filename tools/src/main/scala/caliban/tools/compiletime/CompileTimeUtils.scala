package caliban.tools.compiletime

import caliban.tools.CalibanCommonSettings
import zio.prelude.{ Equivalence, Subtype }

private[caliban] object CompileTimeUtils {
  private def emptyValueMarker: String = "_"

  object NoSpaceString extends Subtype[String]
  type NoSpaceString = NoSpaceString.Type

  private val noSpaceStringEquivalence: Equivalence[NoSpaceString, String] = {
    val marker = "##"

    Equivalence[NoSpaceString, String](
      to = (nps: NoSpaceString) => nps.replaceAll(marker, " "),
      from = (s: String) => NoSpaceString(s.replaceAll(" ", marker))
    )
  }

  private def optionEquivalence[A](fromString: String => A): Equivalence[Option[A], String] =
    Equivalence[Option[A], String](
      to = _.fold(emptyValueMarker)(_.toString),
      from = s => if (s == emptyValueMarker) None else Some(fromString(s))
    )

  private val optionBooleanEquivalence: Equivalence[Option[Boolean], String] = optionEquivalence[Boolean](_.toBoolean)
  private val optionStringEquivalence: Equivalence[Option[String], String]   = optionEquivalence[String](identity)

  private val tupleStringEquivalence: Equivalence[(String, String), String] = {
    val pattern  = "^\\(\\((.+),,(.+)\\)\\)$".r
    val noSpaces = noSpaceStringEquivalence.from
    val noMarker = noSpaceStringEquivalence.to

    Equivalence[(String, String), String](
      to = { case (a, b) => s"((${noSpaces(a)},,${noSpaces(b)}))" },
      from = { case pattern(a, b) => noMarker(NoSpaceString(a)) -> noMarker(NoSpaceString(b)) }
    )
  }

  val calibanCommonSettingsEquivalence: Equivalence[CalibanCommonSettings, List[String]] =
    Equivalence[CalibanCommonSettings, List[String]](
      to = (s: CalibanCommonSettings) =>
        List(
          optionStringEquivalence.to(s.clientName),
          optionStringEquivalence.to(s.scalafmtPath),
          optionStringEquivalence.to(s.packageName),
          optionBooleanEquivalence.to(s.genView),
          optionBooleanEquivalence.to(s.splitFiles),
          optionBooleanEquivalence.to(s.enableFmt),
          optionBooleanEquivalence.to(s.extensibleEnums),
          s.headers.size.toString,
          s.scalarMappings.size.toString,
          s.imports.size.toString
        ) ++ s.headers.map(tupleStringEquivalence.to)
          ++ s.scalarMappings.map(tupleStringEquivalence.to)
          ++ s.imports.map(s => noSpaceStringEquivalence.from(s.trim)),
      from = (args: List[String]) => {
        val List(clientName, scalafmtPath, packageName, genView, splitFiles, enableFmt, extensibleEnums) =
          args.slice(0, 7)

        val List(headersSize, scalarMappingsSize, importsSize) = args.slice(7, 10).map(_.toInt)

        val headers: Seq[(String, String)] =
          if (headersSize > 0) args.slice(10, 10 + headersSize).map(tupleStringEquivalence.from)
          else Seq.empty

        val scalarMappings: Seq[(String, String)] =
          if (scalarMappingsSize > 0)
            args.slice(10 + headersSize, 10 + headersSize + scalarMappingsSize).map(tupleStringEquivalence.from)
          else Seq.empty

        val imports: Seq[String] =
          if (importsSize > 0)
            args
              .slice(10 + headersSize + scalarMappingsSize, 10 + headersSize + scalarMappingsSize + importsSize)
              .map(s => noSpaceStringEquivalence.to(NoSpaceString(s)))
          else Seq.empty

        CalibanCommonSettings(
          clientName = optionStringEquivalence.from(clientName),
          scalafmtPath = optionStringEquivalence.from(scalafmtPath),
          headers = headers,
          packageName = optionStringEquivalence.from(packageName),
          genView = optionBooleanEquivalence.from(genView),
          scalarMappings = scalarMappings,
          imports = imports,
          splitFiles = optionBooleanEquivalence.from(splitFiles),
          enableFmt = optionBooleanEquivalence.from(enableFmt),
          extensibleEnums = optionBooleanEquivalence.from(extensibleEnums)
        )
      }
    )

}
