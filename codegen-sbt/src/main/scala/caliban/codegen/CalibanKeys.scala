package caliban.codegen

import sbt._
import sbt.Keys._

trait CalibanKeys {
  lazy val caliban                                                                              = taskKey[Seq[File]]("Generate GraphQL sources using caliban-codegen-sbt")
  lazy val calibanGenerator                                                                     = taskKey[Seq[File]]("Generate GraphQL sources using caliban-codegen-sbt")
  lazy val calibanSources                                                                       = settingKey[File]("Where to find .graphql schemas")
  lazy val calibanSettings                                                                      = settingKey[Seq[(File, CalibanSettings)]]("Settings that apply to individual GraphQL files")
  def calibanSetting(file: File)(setting: CalibanSettings.Transformer): (File, CalibanSettings) =
    file -> setting.apply(CalibanSettings.empty)
}

object CalibanKeys extends CalibanKeys
