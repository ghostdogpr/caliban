import _root_.caliban.codegen.Codegen
import scala.sys.process._

lazy val base = project
  .in(file("modules/base"))
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban" % Version.pluginVersion
    )
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(CalibanPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban" % Version.pluginVersion
    ),
    Compile / caliban / calibanSettings ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq(
            calibanSetting(file("src/main/graphql/schema.graphql"))(
              _.genType(Codegen.GenType.Schema)
                .clientName("GeneratedAPI")
                .packageName("graphql")
                .effect("MyZQuery")
                .scalarMapping("ID" -> "String")
                .addDerives(true)
                .envForDerives("graphql.Env")
            )
          )
        case _            =>
          Seq(
            calibanSetting(file("src/main/graphql/schema.graphql"))(
              _.genType(Codegen.GenType.Schema)
                .clientName("GeneratedAPI")
                .packageName("graphql")
                .effect("MyZQuery")
                .scalarMapping("ID" -> "String")
                .addDerives(false)
            )
          )
      }
    },
    TaskKey[Unit]("check") := {
      def exists(file: File): Unit =
        if (!file.exists()) throw new MessageOnlyException(s"File does not exist: $file")

      def verify(str: String, file: File): Unit = {
        val cmd = Seq("sh", (baseDirectory.value / "verify.sh").toString, str, file.toString)
        val code = cmd.!
        if (code != 0) throw new MessageOnlyException(s"Verification script failed: ${cmd.mkString(" ")}")
      }

      val generatedFile = (caliban / sourceManaged).value / "main" / "caliban-codegen-sbt" / "graphql" / "GeneratedAPI.scala"

      exists(generatedFile)
      verify("Foo", generatedFile)
      verify("FooInput", generatedFile)
      verify("CustomId", generatedFile)
      verify("FooLazy", generatedFile)
    }
  )
  .dependsOn(base)
