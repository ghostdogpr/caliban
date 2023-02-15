package caliban.codegen

import caliban.tools.CalibanCommonSettings
import caliban.tools.Codegen.GenType

import java.io.File
import java.net.URL

sealed trait CalibanSettings {
  type Self <: CalibanSettings
  def withSettings(f: CalibanCommonSettings => CalibanCommonSettings): Self

  final def clientName(value: String): Self                       = withSettings(_.clientName(value))
  final def scalafmtPath(path: String): Self                      = withSettings(_.scalafmtPath(path))
  final def packageName(name: String): Self                       = withSettings(_.packageName(name))
  final def genView(value: Boolean): Self                         = withSettings(_.genView(value))
  final def scalarMapping(mapping: (String, String)*): Self       =
    withSettings(_.scalarMappings(mapping: _*))
  final def imports(values: String*): Self                        = withSettings(_.imports(values: _*))
  final def splitFiles(value: Boolean): Self                      = withSettings(_.splitFiles(value))
  final def enableFmt(value: Boolean): Self                       = withSettings(_.enableFmt(value))
  final def extensibleEnums(value: Boolean): Self                 = withSettings(_.extensibleEnums(value))
  final def genType(genType: GenType): Self                       = withSettings(_.genType(genType))
  final def effect(effect: String): Self                          = withSettings(_.effect(effect))
  final def abstractEffectType(abstractEffectType: Boolean): Self =
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
