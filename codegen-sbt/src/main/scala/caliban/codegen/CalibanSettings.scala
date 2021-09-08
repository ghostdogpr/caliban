package caliban.codegen

import caliban.tools.Options
import monocle.macros.GenLens
import zio.prelude._

import java.io.File
import java.net.URL

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

  def toOptions(schemaPath: String, toPath: String): Options =
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

sealed trait CalibanSettings

final case class CalibanFileSettings(file: File, settings: CalibanCommonSettings) extends CalibanSettings {
  import CalibanFileSettings._

  def clientName(value: String): CalibanFileSettings                 = clientNameLens.set(Some(value))(this)
  def scalafmtPath(path: String): CalibanFileSettings                = scalafmtPathLens.set(Some(path))(this)
  def packageName(name: String): CalibanFileSettings                 = packageNameLens.set(Some(name))(this)
  def genView(value: Boolean): CalibanFileSettings                   = genViewLens.set(Some(value))(this)
  def scalarMapping(mapping: (String, String)*): CalibanFileSettings = scalarMappingsLens.modify(_ ++ mapping)(this)
  def imports(values: String*): CalibanFileSettings                  = importsLens.modify(_ ++ values)(this)
  def splitFiles(value: Boolean): CalibanFileSettings                = splitFilesLens.set(Some(value))(this)
  def enableFmt(value: Boolean): CalibanFileSettings                 = enableFmtLens.set(Some(value))(this)
  def extensibleEnums(value: Boolean): CalibanFileSettings           = extensibleEnumsLens.set(Some(value))(this)
}
object CalibanFileSettings {
  private[CalibanFileSettings] val clientNameLens      = GenLens[CalibanFileSettings](_.settings.clientName)
  private[CalibanFileSettings] val scalafmtPathLens    = GenLens[CalibanFileSettings](_.settings.scalafmtPath)
  private[CalibanFileSettings] val packageNameLens     = GenLens[CalibanFileSettings](_.settings.packageName)
  private[CalibanFileSettings] val genViewLens         = GenLens[CalibanFileSettings](_.settings.genView)
  private[CalibanFileSettings] val scalarMappingsLens  = GenLens[CalibanFileSettings](_.settings.scalarMappings)
  private[CalibanFileSettings] val importsLens         = GenLens[CalibanFileSettings](_.settings.imports)
  private[CalibanFileSettings] val splitFilesLens      = GenLens[CalibanFileSettings](_.settings.splitFiles)
  private[CalibanFileSettings] val enableFmtLens       = GenLens[CalibanFileSettings](_.settings.enableFmt)
  private[CalibanFileSettings] val extensibleEnumsLens = GenLens[CalibanFileSettings](_.settings.extensibleEnums)
}

final case class CalibanUrlSettings(url: URL, settings: CalibanCommonSettings) extends CalibanSettings {
  import CalibanUrlSettings._

  def clientName(value: String): CalibanUrlSettings                 = clientNameLens.set(Some(value))(this)
  def scalafmtPath(path: String): CalibanUrlSettings                = scalafmtPathLens.set(Some(path))(this)
  def headers(mapping: (String, String)*): CalibanUrlSettings       = headersLens.modify(_ ++ mapping)(this)
  def packageName(name: String): CalibanUrlSettings                 = packageNameLens.set(Some(name))(this)
  def genView(value: Boolean): CalibanUrlSettings                   = genViewLens.set(Some(value))(this)
  def scalarMapping(mapping: (String, String)*): CalibanUrlSettings = scalarMappingsLens.modify(_ ++ mapping)(this)
  def imports(values: String*): CalibanUrlSettings                  = importsLens.modify(_ ++ values)(this)
  def splitFiles(value: Boolean): CalibanUrlSettings                = splitFilesLens.set(Some(value))(this)
  def enableFmt(value: Boolean): CalibanUrlSettings                 = enableFmtLens.set(Some(value))(this)
  def extensibleEnums(value: Boolean): CalibanUrlSettings           = extensibleEnumsLens.set(Some(value))(this)
}
object CalibanUrlSettings {
  private[CalibanUrlSettings] val clientNameLens      = GenLens[CalibanUrlSettings](_.settings.clientName)
  private[CalibanUrlSettings] val scalafmtPathLens    = GenLens[CalibanUrlSettings](_.settings.scalafmtPath)
  private[CalibanUrlSettings] val headersLens         = GenLens[CalibanUrlSettings](_.settings.headers)
  private[CalibanUrlSettings] val packageNameLens     = GenLens[CalibanUrlSettings](_.settings.packageName)
  private[CalibanUrlSettings] val genViewLens         = GenLens[CalibanUrlSettings](_.settings.genView)
  private[CalibanUrlSettings] val scalarMappingsLens  = GenLens[CalibanUrlSettings](_.settings.scalarMappings)
  private[CalibanUrlSettings] val importsLens         = GenLens[CalibanUrlSettings](_.settings.imports)
  private[CalibanUrlSettings] val splitFilesLens      = GenLens[CalibanUrlSettings](_.settings.splitFiles)
  private[CalibanUrlSettings] val enableFmtLens       = GenLens[CalibanUrlSettings](_.settings.enableFmt)
  private[CalibanUrlSettings] val extensibleEnumsLens = GenLens[CalibanUrlSettings](_.settings.extensibleEnums)
}
