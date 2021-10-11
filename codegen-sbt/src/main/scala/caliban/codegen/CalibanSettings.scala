package caliban.codegen

import caliban.tools.CalibanCommonSettings

import java.io.File
import java.net.URL

sealed trait CalibanSettings

final case class CalibanFileSettings(file: File, settings: CalibanCommonSettings) extends CalibanSettings {

  def clientName(value: String): CalibanFileSettings                 = this.copy(settings = this.settings.clientName(value))
  def scalafmtPath(path: String): CalibanFileSettings                = this.copy(settings = this.settings.scalafmtPath(path))
  def packageName(name: String): CalibanFileSettings                 = this.copy(settings = this.settings.packageName(name))
  def genView(value: Boolean): CalibanFileSettings                   = this.copy(settings = this.settings.genView(value))
  def scalarMapping(mapping: (String, String)*): CalibanFileSettings =
    this.copy(settings = this.settings.scalarMappings(mapping: _*))
  def imports(values: String*): CalibanFileSettings                  = this.copy(settings = this.settings.imports(values: _*))
  def splitFiles(value: Boolean): CalibanFileSettings                = this.copy(settings = this.settings.splitFiles(value))
  def enableFmt(value: Boolean): CalibanFileSettings                 = this.copy(settings = this.settings.enableFmt(value))
  def extensibleEnums(value: Boolean): CalibanFileSettings           = this.copy(settings = this.settings.extensibleEnums(value))
  def excludeClientDeprecation(value: Boolean): CalibanFileSettings  =
    this.copy(settings = this.settings.excludeClientDeprecation(value))
}

final case class CalibanUrlSettings(url: URL, settings: CalibanCommonSettings) extends CalibanSettings {

  def clientName(value: String): CalibanUrlSettings                 = this.copy(settings = this.settings.clientName(value))
  def scalafmtPath(path: String): CalibanUrlSettings                = this.copy(settings = this.settings.scalafmtPath(path))
  def headers(values: (String, String)*): CalibanUrlSettings        = this.copy(settings = this.settings.headers(values: _*))
  def packageName(name: String): CalibanUrlSettings                 = this.copy(settings = this.settings.packageName(name))
  def genView(value: Boolean): CalibanUrlSettings                   = this.copy(settings = this.settings.genView(value))
  def scalarMapping(mapping: (String, String)*): CalibanUrlSettings =
    this.copy(settings = this.settings.scalarMappings(mapping: _*))
  def imports(values: String*): CalibanUrlSettings                  = this.copy(settings = this.settings.imports(values: _*))
  def splitFiles(value: Boolean): CalibanUrlSettings                = this.copy(settings = this.settings.splitFiles(value))
  def enableFmt(value: Boolean): CalibanUrlSettings                 = this.copy(settings = this.settings.enableFmt(value))
  def extensibleEnums(value: Boolean): CalibanUrlSettings           = this.copy(settings = this.settings.extensibleEnums(value))
  def excludeClientDepercation(value: Boolean): CalibanUrlSettings  =
    this.copy(settings = this.settings.excludeClientDeprecation(value))
}
