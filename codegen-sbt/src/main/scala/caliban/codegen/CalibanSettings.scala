package caliban.codegen

import caliban.tools.CalibanCommonSettings
import caliban.tools.Codegen.GenType

import java.io.File
import java.net.URL

sealed trait CalibanSettings {
  type Self <: CalibanSettings
  def withSettings(f: CalibanCommonSettings => CalibanCommonSettings): Self

  def clientName(value: String): Self                       = withSettings(_.clientName(value))
  def scalafmtPath(path: String): Self                      = withSettings(_.scalafmtPath(path))
  def packageName(name: String): Self                       = withSettings(_.packageName(name))
  def genView(value: Boolean): Self                         = withSettings(_.genView(value))
  def scalarMapping(mapping: (String, String)*): Self       =
    withSettings(_.scalarMappings(mapping: _*))
  def imports(values: String*): Self                        = withSettings(_.imports(values: _*))
  def splitFiles(value: Boolean): Self                      = withSettings(_.splitFiles(value))
  def enableFmt(value: Boolean): Self                       = withSettings(_.enableFmt(value))
  def extensibleEnums(value: Boolean): Self                 = withSettings(_.extensibleEnums(value))
  def genType(genType: GenType): Self                       = withSettings(_.genType(genType))
  def effect(effect: String): Self                          = withSettings(_.effect(effect))
  def abstractEffectType(abstractEffectType: Boolean): Self =
    withSettings(_.abstractEffectType(abstractEffectType))
}

final case class CalibanFileSettings(file: File, settings: CalibanCommonSettings) extends CalibanSettings {
  type Self = CalibanFileSettings
  def withSettings(f: CalibanCommonSettings => CalibanCommonSettings): Self =
    this.copy(settings = f(settings))
}

final case class CalibanUrlSettings(url: URL, settings: CalibanCommonSettings) extends CalibanSettings {
  type Self = CalibanUrlSettings
  def withSettings(f: CalibanCommonSettings => CalibanCommonSettings): Self =
    this.copy(settings = f(settings))

  def headers(values: (String, String)*): CalibanUrlSettings = this.copy(settings = this.settings.headers(values: _*))
}
