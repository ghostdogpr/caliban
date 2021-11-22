package caliban.codegen

import caliban.tools.CalibanCommonSettings
import sbt._

import java.net.URL

trait CalibanKeys {
  lazy val caliban          = taskKey[Seq[File]]("Generate GraphQL sources using caliban-codegen-sbt")
  lazy val calibanGenerator = taskKey[Seq[File]]("Generate GraphQL sources using caliban-codegen-sbt")

  lazy val calibanSources  = settingKey[File]("Where to find .graphql schemas")
  lazy val calibanSettings = settingKey[Seq[CalibanSettings]]("Settings that apply to individual GraphQL files")

  def calibanSetting(file: File)(setting: CalibanFileSettings => CalibanFileSettings): CalibanSettings =
    setting.apply(CalibanFileSettings(file = file, settings = CalibanCommonSettings.empty))
  def calibanSetting(url: URL)(setting: CalibanUrlSettings => CalibanUrlSettings): CalibanSettings     =
    setting.apply(CalibanUrlSettings(url = url, settings = CalibanCommonSettings.empty))

  @deprecated("CodegenPlugin has been renamed to CalibanPlugin", "1.1.0")
  val CodegenPlugin: CalibanPlugin.type = CalibanPlugin

  val calibanVersion = settingKey[String]("Version of the Caliban sbt plugin")
}

object CalibanKeys extends CalibanKeys
