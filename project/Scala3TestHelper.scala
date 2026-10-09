import sbt._
import Keys.{ scalaVersion, _ }

object Scala3TestPlugin extends AutoPlugin {
  val scala3TestPluginVersion = "test-codegen-sbt-compile-scala3"
  val scala212Text            = "2.12"

  override def trigger = allRequirements

  override lazy val projectSettings = Seq(
    commands ++= Seq(codegenScriptedScala3)
  )

  lazy val codegenScriptedScala3 = Command.command("codegenScriptedScala3") { state =>
    val crossVersions       = state.setting(ThisBuild / crossScalaVersions)
    val scala212VersionText = crossVersions
      .find(_.startsWith(scala212Text))
      .getOrElse(throw new Exception("Cannot find Scala 2.12 version in ThisBuild / crossScalaVersions"))
    val scala3VersionText   = crossVersions
      .find(_.startsWith("3."))
      .getOrElse(throw new Exception("Cannot find Scala 3 version in ThisBuild / crossScalaVersions"))
    // Publish the Scala 3 libraries explicitly: codegenSbt/scripted publishes the Scala 2.12 ones itself,
    // and the scripted builds don't need Scaladoc.
    val newState            = Command.process(
      s"""set ThisBuild / version := "${scala3TestPluginVersion}";""" +
        "set ThisBuild / Compile / packageDoc / publishArtifact := false;" +
        s"++$scala3VersionText; all macros/publishLocal core/publishLocal clientJVM/publishLocal tools/publishLocal codegen/publishLocal;" +
        s"++$scala212VersionText; codegenSbt/scripted",
      state,
      msg => throw new Exception("Error while parsing SBT command: " + msg)
    )
    newState
  }
}
