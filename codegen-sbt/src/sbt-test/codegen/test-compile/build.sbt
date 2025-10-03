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
        _.clientName("Client")
      ),
      // Another entry for the same file, which will cause another generator to run
      calibanSetting(file("src/main/graphql/schema.graphql"))(
        _.genType(Codegen.GenType.Schema)
          .scalarMapping("Json" -> "String")
          .effect("scala.util.Try")
          .addDerives(false)
      ),
      calibanSetting(file("src/main/graphql/schema.graphql"))(
        _.genType(Codegen.GenType.Schema)
          .scalarMapping("Json" -> "String")
          .effect("F")
          .abstractEffectType(true)
      ),
      calibanSetting(file("src/main/graphql/genview/schema.graphql"))(
        _.clientName("Client").packageName("genview").genView(true)
      )
    ),
    TaskKey[Unit]("check") := {
      def exists(file: File): Unit =
        if (!file.exists()) throw new MessageOnlyException(s"File does not exist: $file")

      def verify(str: String, file: File): Unit = {
        val cmd = Seq("sh", (baseDirectory.value / "verify.sh").toString, str, file.toString)
        val code = cmd.!
        if (code != 0) throw new MessageOnlyException(s"Verification script failed: ${cmd.mkString(" ")}")
      }

      val generatedDir = (caliban / sourceManaged).value / "main" / "caliban-codegen-sbt"
      val genviewClient = generatedDir / "genview" / "Client.scala"

      exists(generatedDir / "caliban" / "Client.scala")
      exists(genviewClient)
      exists(generatedDir / "caliban" / "schema.scala")

      verify("CharacterView", genviewClient)
      verify("OptionView", genviewClient)
      verify("CharacterOneOfInput", genviewClient)
    }
  )
