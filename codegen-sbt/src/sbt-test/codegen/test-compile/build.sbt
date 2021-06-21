lazy val root = project
  .in(file("."))
  .enablePlugins(CodegenPlugin)  // Intentionally maintain the deprecated name
  .settings(
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban-client" % Version.pluginVersion
    ),
    Compile / caliban / calibanSettings ++= Seq(
      calibanSetting(file("src/main/graphql/schema.graphql"))(  // Explicitly constrain to disambiguate
        cs =>
          cs.clientName("Client")
      ),
      calibanSetting(file("src/main/graphql/genview/schema.graphql"))(
        cs =>
          cs.clientName("Client")
            .packageName("genview")
            .genView(true)
      )
    )
  )
