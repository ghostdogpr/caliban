import _root_.caliban.tools.Codegen

lazy val root = project
  .in(file("."))
  .enablePlugins(CalibanPlugin)
  .settings(
    libraryDependencies ++= Seq(
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
      ),
      calibanSetting(file("src/main/graphql/genview/schema.graphql"))(
        _.clientName("Client").packageName("genview").genView(true)
      )
    )
  )
