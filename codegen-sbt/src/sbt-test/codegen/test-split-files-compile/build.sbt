import _root_.caliban.codegen.Codegen
import scala.sys.process._

lazy val root = project
  .in(file("."))
  .enablePlugins(CalibanPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban"        % Version.pluginVersion,
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    ),
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("src/main/graphql/schema.graphql"))( // Explicitly constrain to disambiguate
        _.splitFiles(true)
      )
    ),
    TaskKey[Unit]("check") := {
      def exists(file: File): Unit =
        if (!file.exists()) throw new MessageOnlyException(s"File does not exist: $file")

      val generatedDir = (caliban / sourceManaged).value / "main" / "caliban-codegen-sbt" / "caliban"

      exists(generatedDir / "package.scala")
      exists(generatedDir / "Character.scala")
      exists(generatedDir / "Canterbury.scala")
    }
  )
