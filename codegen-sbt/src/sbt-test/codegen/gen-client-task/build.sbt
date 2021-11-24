lazy val root = project
  .in(file("."))
  .enablePlugins(CodegenPlugin) // Intentionally maintain the deprecated name
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    )
  )
