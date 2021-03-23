lazy val root = project
  .in(file("."))
  .enablePlugins(CodegenPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    )
  )
