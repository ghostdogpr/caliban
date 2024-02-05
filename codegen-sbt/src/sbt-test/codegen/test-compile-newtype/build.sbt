import _root_.caliban.tools.Codegen

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
    }
  )
  .dependsOn(base)
