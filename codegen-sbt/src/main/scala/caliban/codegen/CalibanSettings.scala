package caliban.codegen

import caliban.tools.CalibanCommonSettings
import monocle.macros.GenLens

import java.io.File
import java.net.URL

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
