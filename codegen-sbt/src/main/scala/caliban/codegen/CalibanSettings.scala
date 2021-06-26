package caliban.codegen

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

  def append(other: Type): Type
}

case class CalibanFileSettings(
  file: File,
  clientName: Option[String],
  scalafmtPath: Option[String],
  packageName: Option[String],
  genView: Option[Boolean],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String]
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
      imports = imports ++ other.imports
    )

  def clientName(value: String): CalibanFileSettings                 = this.copy(clientName = Some(value))
  def scalafmtPath(path: String): CalibanFileSettings                = this.copy(scalafmtPath = Some(path))
  def packageName(name: String): CalibanFileSettings                 = this.copy(packageName = Some(name))
  def genView(value: Boolean): CalibanFileSettings                   = this.copy(genView = Some(value))
  def scalarMapping(mapping: (String, String)*): CalibanFileSettings =
    this.copy(scalarMappings = this.scalarMappings ++ mapping)
  def imports(values: String*): CalibanFileSettings                  = this.copy(imports = this.imports ++ values)
}

case class CalibanUrlSettings(
  url: URL,
  clientName: Option[String],
  scalafmtPath: Option[String],
  headers: Seq[(String, String)],
  packageName: Option[String],
  genView: Option[Boolean],
  scalarMappings: Seq[(String, String)],
  imports: Seq[String]
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
      imports = imports ++ other.imports
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
}

object CalibanSettings {
  type Transformer = CalibanSettings => CalibanSettings
  def emptyFile(file: File) = CalibanFileSettings(
    file = file,
    clientName = Option.empty[String],
    scalafmtPath = Option.empty[String],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String]
  )

  def emptyUrl(url: URL) = CalibanUrlSettings(
    url = url,
    clientName = Option.empty[String],
    scalafmtPath = Option.empty[String],
    headers = Seq.empty[(String, String)],
    packageName = Option.empty[String],
    genView = Option.empty[Boolean],
    scalarMappings = Seq.empty[(String, String)],
    imports = Seq.empty[String]
  )
}
