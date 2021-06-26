package caliban.codegen

import sbt._
import sbt.Keys._

object CalibanPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin

  object autoImport extends CalibanKeys
  import autoImport._

  lazy val baseSettings = Seq(
    caliban := (caliban / calibanGenerator).value,
    (caliban / sourceManaged) := {
      sourceManaged.value / "caliban-codegen-sbt"
    },
    (caliban / calibanSources) := {
      if (Seq(Compile, Test).contains(configuration.value)) sourceDirectory.value / "graphql"
      else sourceDirectory.value / "main" / "graphql"
    },
    caliban / calibanSettings := Seq.empty
  )

  lazy val calibanScopedSettings = inTask(caliban)(
    Seq(
      sources := (calibanSources.value ** "*.graphql").get.sorted,
      clean := {
        val sourceDir = sourceManaged.value
        IO.delete((sourceDir ** "*").get)
        IO.createDirectory(sourceDir)
      },
      calibanGenerator := CalibanSourceGenerator(
        calibanSources.value,
        sources.value,
        sourceManaged.value,
        streams.value.cacheDirectory,
        calibanSettings.value.collect { case x: CalibanFileSettings => x },
        calibanSettings.value.collect { case x: CalibanUrlSettings => x }
      )
    )
  )

  lazy val allSettings = baseSettings ++ calibanScopedSettings

  override lazy val projectSettings: Seq[Def.Setting[_]] =
    CalibanCli.projectSettings ++ inConfig(Compile)(allSettings) ++ inConfig(Test)(allSettings) ++ Seq(
      Compile / sourceGenerators += (Compile / caliban).taskValue,
      Test / sourceGenerators += (Test / caliban).taskValue
    )
}
