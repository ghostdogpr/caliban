lazy val root = project
  .in(file("."))
  .enablePlugins(CalibanPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    )
  )
