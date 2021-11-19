package caliban.tools

import caliban.tools.Codegen.GenType

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
  extensibleEnums: Option[Boolean],
  genType: GenType,
  effect: Option[String],
  abstractEffectType: Option[Boolean]
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
      effect = effect,
      scalarMappings = Option(scalarMappings.toMap).filter(_.nonEmpty),
      imports = Option(imports.toList).filter(_.nonEmpty),
      abstractEffectType = abstractEffectType,
      splitFiles = splitFiles,
      enableFmt = enableFmt,
      extensibleEnums = extensibleEnums
    )

  private[caliban] def combine(r: => CalibanCommonSettings): CalibanCommonSettings =
    CalibanCommonSettings(
      clientName = r.clientName.orElse(this.clientName),
      scalafmtPath = r.scalafmtPath.orElse(this.scalafmtPath),
      headers = this.headers ++ r.headers,
      packageName = r.packageName.orElse(this.packageName),
      genView = r.genView.orElse(this.genView),
      scalarMappings = this.scalarMappings ++ r.scalarMappings,
      imports = this.imports ++ r.imports,
      splitFiles = r.splitFiles.orElse(this.splitFiles),
      enableFmt = r.enableFmt.orElse(this.enableFmt),
      extensibleEnums = r.extensibleEnums.orElse(this.extensibleEnums),
      genType = r.genType,
      effect = r.effect.orElse(this.effect),
      abstractEffectType = r.abstractEffectType.orElse(this.abstractEffectType)
    )

  def clientName(value: String): CalibanCommonSettings                       = this.copy(clientName = Some(value))
  def scalafmtPath(value: String): CalibanCommonSettings                     = this.copy(scalafmtPath = Some(value))
  def headers(headers: (String, String)*): CalibanCommonSettings             = this.copy(headers = this.headers ++ headers)
  def packageName(name: String): CalibanCommonSettings                       = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanCommonSettings                         = this.copy(genView = Some(value))
  def scalarMappings(mapping: (String, String)*): CalibanCommonSettings      =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(imports: String*): CalibanCommonSettings                       = this.copy(imports = this.imports ++ imports)
  def splitFiles(value: Boolean): CalibanCommonSettings                      = this.copy(splitFiles = Some(value))
  def enableFmt(value: Boolean): CalibanCommonSettings                       = this.copy(enableFmt = Some(value))
  def extensibleEnums(value: Boolean): CalibanCommonSettings                 = this.copy(extensibleEnums = Some(value))
  def genType(genType: GenType): CalibanCommonSettings                       = this.copy(genType = genType)
  def effect(effect: String): CalibanCommonSettings                          = this.copy(effect = Some(effect))
  def abstractEffectType(abstractEffectType: Boolean): CalibanCommonSettings =
    this.copy(abstractEffectType = Some(abstractEffectType))
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
      extensibleEnums = None,
      genType = GenType.Client,
      effect = None,
      abstractEffectType = None
    )
}
