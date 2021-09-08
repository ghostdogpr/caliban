package caliban.tools

import zio.prelude.Associative

final case class CalibanCommonSettings(
  clientName: Option[String],
  scalafmtPath: Option[String],
  headers: Seq[(String, String)],
  packageName: Option[String],
  genView: Option[Boolean],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String],
  splitFiles: Option[Boolean],
  enableFmt: Option[Boolean],
  extensibleEnums: Option[Boolean]
) {

  private[caliban] def toOptions(schemaPath: String, toPath: String): Options =
    Options(
      schemaPath = schemaPath,
      toPath = toPath,
      fmtPath = scalafmtPath,
      headers = Option(headers.map((Options.Header.apply _).tupled).toList).filter(_.nonEmpty),
      packageName = packageName,
      clientName = clientName,
      genView = genView,
      effect = Option.empty,
      scalarMappings = Option(scalarMappings.toMap).filter(_.nonEmpty),
      imports = Option(imports.toList).filter(_.nonEmpty),
      abstractEffectType = Option.empty,
      splitFiles = splitFiles,
      enableFmt = enableFmt,
      extensibleEnums = extensibleEnums
    )

  def clientName(value: String): CalibanCommonSettings                  = this.copy(clientName = Some(value))
  def scalafmtPath(value: String): CalibanCommonSettings                = this.copy(scalafmtPath = Some(value))
  def headers(headers: (String, String)*): CalibanCommonSettings        = this.copy(headers = this.headers ++ headers)
  def packageName(name: String): CalibanCommonSettings                  = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanCommonSettings                    = this.copy(genView = Some(value))
  def scalarMappings(mapping: (String, String)*): CalibanCommonSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(imports: String*): CalibanCommonSettings                  = this.copy(imports = this.imports ++ imports)
  def splitFiles(value: Boolean): CalibanCommonSettings                 = this.copy(splitFiles = Some(value))
  def enableFmt(value: Boolean): CalibanCommonSettings                  = this.copy(enableFmt = Some(value))
  def extensibleEnums(value: Boolean): CalibanCommonSettings            = this.copy(extensibleEnums = Some(value))
}

object CalibanCommonSettings {
  val empty: CalibanCommonSettings =
    CalibanCommonSettings(
      clientName = None,
      scalafmtPath = None,
      headers = Seq.empty,
      packageName = None,
      genView = None,
      scalarMappings = Seq.empty,
      imports = Seq.empty,
      splitFiles = None,
      enableFmt = None,
      extensibleEnums = None
    )

  implicit val associative: Associative[CalibanCommonSettings] =
    new Associative[CalibanCommonSettings] {
      override def combine(l: => CalibanCommonSettings, r: => CalibanCommonSettings): CalibanCommonSettings =
        CalibanCommonSettings(
          clientName = r.clientName.orElse(l.clientName),
          scalafmtPath = r.scalafmtPath.orElse(l.scalafmtPath),
          headers = l.headers ++ r.headers,
          packageName = r.packageName.orElse(l.packageName),
          genView = r.genView.orElse(l.genView),
          scalarMappings = l.scalarMappings ++ r.scalarMappings,
          imports = l.imports ++ r.imports,
          splitFiles = r.splitFiles.orElse(l.splitFiles),
          enableFmt = r.enableFmt.orElse(l.enableFmt),
          extensibleEnums = r.extensibleEnums.orElse(l.extensibleEnums)
        )
    }
}
