package caliban.codegen

import caliban.tools.Options

import java.io.File
import java.net.URL

sealed trait CalibanSettings {
  type Type <: CalibanSettings

  def clientName: Option[String]
  def scalafmtPath: Option[String]
  def headers: Seq[(String, String)]
  def packageName: Option[String]
  def genView: Option[Boolean]
  def scalarMappings: Seq[(String, String)]
  def imports: Seq[String]
  def splitFiles: Option[Boolean]
  def enableFmt: Option[Boolean]
  def extensibleEnums: Option[Boolean]

  def append(other: Type): Type

  def toOptions(schemaPath: String, toPath: String) = Options(
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
}

case class CalibanFileSettings(
  file: File,
  clientName: Option[String],
  scalafmtPath: Option[String],
  packageName: Option[String],
  genView: Option[Boolean],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String],
  splitFiles: Option[Boolean],
  enableFmt: Option[Boolean],
  extensibleEnums: Option[Boolean]
) extends CalibanSettings {
  type Type = CalibanFileSettings
  val headers = Seq.empty // Not applicable for file generator

  def append(other: CalibanFileSettings): CalibanFileSettings =
    CalibanFileSettings(
      file = file,
      clientName = other.clientName.orElse(clientName),
      scalafmtPath = other.scalafmtPath.orElse(scalafmtPath),
      packageName = other.packageName.orElse(packageName),
      genView = other.genView.orElse(genView),
      scalarMappings = scalarMappings ++ other.scalarMappings,
      imports = imports ++ other.imports,
      splitFiles = other.splitFiles.orElse(splitFiles),
      enableFmt = other.enableFmt.orElse(enableFmt),
      extensibleEnums = other.extensibleEnums.orElse(extensibleEnums)
    )

  def clientName(value: String): CalibanFileSettings                 = this.copy(clientName = Some(value))
  def scalafmtPath(path: String): CalibanFileSettings                = this.copy(scalafmtPath = Some(path))
  def packageName(name: String): CalibanFileSettings                 = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanFileSettings                   = this.copy(genView = Some(value))
  def scalarMapping(mapping: (String, String)*): CalibanFileSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(values: String*): CalibanFileSettings                  = this.copy(imports = this.imports ++ values)
  def splitFiles(value: Boolean): CalibanFileSettings                = this.copy(splitFiles = Some(value))
  def enableFmt(value: Boolean): CalibanFileSettings                 = this.copy(enableFmt = Some(value))
  def extensibleEnums(value: Boolean): CalibanFileSettings           = this.copy(extensibleEnums = Some(value))
}

case class CalibanUrlSettings(
  url: URL,
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
) extends CalibanSettings {
  type Type = CalibanUrlSettings
  def append(other: CalibanUrlSettings): CalibanUrlSettings =
    CalibanUrlSettings(
      url = url,
      clientName = other.clientName.orElse(clientName),
      scalafmtPath = other.scalafmtPath.orElse(scalafmtPath),
      headers = headers ++ other.headers,
      packageName = other.packageName.orElse(packageName),
      genView = other.genView.orElse(genView),
      scalarMappings = scalarMappings ++ other.scalarMappings,
      imports = imports ++ other.imports,
      splitFiles = other.splitFiles.orElse(splitFiles),
      enableFmt = other.enableFmt.orElse(enableFmt),
      extensibleEnums = other.extensibleEnums.orElse(extensibleEnums)
    )

  def clientName(value: String): CalibanUrlSettings                 = this.copy(clientName = Some(value))
  def scalafmtPath(path: String): CalibanUrlSettings                = this.copy(scalafmtPath = Some(path))
  def headers(mapping: (String, String)*): CalibanUrlSettings       =
    this.copy(headers = this.headers ++ mapping)
  def packageName(name: String): CalibanUrlSettings                 = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanUrlSettings                   = this.copy(genView = Some(value))
  def scalarMapping(mapping: (String, String)*): CalibanUrlSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(values: String*): CalibanUrlSettings                  = this.copy(imports = this.imports ++ values)
  def splitFiles(value: Boolean): CalibanUrlSettings                = this.copy(splitFiles = Some(value))
  def enableFmt(value: Boolean): CalibanUrlSettings                 = this.copy(enableFmt = Some(value))
  def extensibleEnums(value: Boolean): CalibanUrlSettings           = this.copy(extensibleEnums = Some(value))
}

object CalibanSettings {
  type Transformer = CalibanSettings => CalibanSettings
  def emptyFile(file: File): CalibanFileSettings = CalibanFileSettings(
    file = file,
    clientName = Option.empty[String],
    scalafmtPath = Option.empty[String],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String],
    splitFiles = Option.empty[Boolean],
    enableFmt = Option.empty[Boolean],
    extensibleEnums = Option.empty[Boolean]
  )

  def emptyUrl(url: URL): CalibanUrlSettings = CalibanUrlSettings(
    url = url,
    clientName = Option.empty[String],
    scalafmtPath = Option.empty[String],
    headers = Seq.empty[(String, String)],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String],
    splitFiles = Option.empty[Boolean],
    enableFmt = Option.empty[Boolean],
    extensibleEnums = Option.empty[Boolean]
  )
}
