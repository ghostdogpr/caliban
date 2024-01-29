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
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("src/main/graphql/schema.graphql"))(
        _.genType(Codegen.GenType.Schema)
          .clientName("GeneratedAPI")
          .packageName("graphql")
          .effect("MyZQuery")
          .scalarMapping("ID" -> "String")
          .addDerives(true)
      )
    )
  )
  .dependsOn(base)
