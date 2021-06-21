lazy val root = project
  .in(file("."))
  .enablePlugins(CodegenPlugin)  // Intentionally maintain the deprecated name
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    ),
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("schema.graphql"))(
        cs =>
          cs.clientName("Client")
      )
    )
  )
