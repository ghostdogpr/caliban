import _root_.caliban.codegen.Codegen

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
    )
  )
