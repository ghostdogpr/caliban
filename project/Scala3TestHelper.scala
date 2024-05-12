import sbt._
import Keys.{ scalaVersion, _ }

object Scala3TestPlugin extends AutoPlugin {
  val scala3TestPluginVersion = "test-codegen-sbt-compile-scala3"
  val scala212Text            = "2.12"

  override def trigger = allRequirements

  object autoImport {
    lazy val changeScalaVersionInTest = inputKey[Unit]("change scala version in test directory for codegen-sbt")
  }

  import autoImport._

  def changeScalaVersionInTestImpl: Def.Initialize[InputTask[Unit]] = Def.inputTask {
    val Seq(originalVersion, newVersion) = Def.spaceDelimited().parsed
    val testFile                         = baseDirectory.value / "src/sbt-test/compiletime-codegen/test-compile/test"
    val content                          = IO.read(testFile)
    IO.write(testFile, content.replace(originalVersion, newVersion))
  }

  override lazy val projectSettings = Seq(
    changeScalaVersionInTest := changeScalaVersionInTestImpl.evaluated, // changeScalaVersionInTestImpl,
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
        s"codegenSbt/changeScalaVersionInTest ${scala212Text} ${scala3VersionText};" +
        s"codegenSbt/scripted; codegenSbt/changeScalaVersionInTest ${scala3VersionText} ${scala212Text}",
      state,
      msg => throw new Exception("Error while parsing SBT command: " + msg)
    )
    newState
  }
}
