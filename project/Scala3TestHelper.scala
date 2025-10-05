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
    val codegenProject      = Project.structure(state).allProjectRefs.find(p => p.project == "codegenSbt").get
    val scala3VersionText   = state
      .setting(ThisBuild / crossScalaVersions)
      .find(_.startsWith("3."))
      .getOrElse(throw new Exception("Cannot find Scala 3 version in ThisBuild / crossScalaVersions"))
    val scalaVersionsToTest = state
      .setting(ThisBuild / crossScalaVersions)
      .filter(s => s.startsWith(scala212Text) || s == scala3VersionText)
      .map(s => s""""${s}"""")
      .mkString(",")
    val newState            = Command.process(
      s"""set ThisBuild / version := "${scala3TestPluginVersion}";""" +
        s"set ThisBuild / crossScalaVersions := Seq(${scalaVersionsToTest}); +publishLocal;" +
        "codegenSbt/scripted",
      state,
      msg => throw new Exception("Error while parsing SBT command: " + msg)
    )
    newState
  }
}
