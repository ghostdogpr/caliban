import sbt._
import Keys.{ scalaVersion, _ }

object Scala3TestHelper {
  val scala3TestPluginVersion = "test-codegen-sbt-compile-scala3"
  val scala3Version           = settingKey[String]("To test sbt codegen with scala3")

  def codegenScriptedScala3 = Command.command("codegenScriptedScala3") { state =>
    val extracted           = Project.extract(state)
    import extracted._
    val codegenProject      = Project.structure(state).allProjectRefs.find(p => p.project == "codegenSbt").get
    val scala3VersionKey    = codegenProject / scala3Version
    val scala3VersionText   = state.setting(scala3VersionKey)
    val scalaVersionsToTest = state
      .setting(ThisBuild / crossScalaVersions)
      .filter(s => s.startsWith("2.12") || s.startsWith("3."))
      .map(s => s""""${s}"""")
      .mkString(",")
    val testFile            = get(baseDirectory) / "codegen-sbt/src/sbt-test/compiletime-codegen/test-compile/test"
    val content             = IO.read(testFile)
    IO.write(testFile, content.replace("2.12", scala3VersionText))
    Command.process(
      s"""set ThisBuild / version := "${scala3TestPluginVersion}";""" +
        s"set ThisBuild / crossScalaVersions := Seq(${scalaVersionsToTest}); +publishLocal; codegenSbt/scripted",
      state
    )
  }
}
