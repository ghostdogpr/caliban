import sbt.Def.spaceDelimited

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "Caliban"
ThisBuild / homepage := Some(url("https://ghostdogpr.github.io/caliban/"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "2.12.14" // Must stay on 2.12 until sbt is using 2.12

lazy val commonSettings = Seq(
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  libraryDependencies ++= tests.map(_ % Test),
  (Test / parallelExecution) := true,
  (Test / fork) := true
)

// ### Dependencies ###

lazy val calibanLib: Seq[ModuleID] =
  sys.props.get("plugin.version") match {
    case Some(x) =>
      Seq(
        "com.github.ghostdogpr" %% "caliban"        % x,
        "com.github.ghostdogpr" %% "caliban-http4s" % x
      )
    case _       => sys.error("""|The system property 'plugin.version' is not defined.
                           |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
  }

val sttpVersion   = "3.3.15"
val http4sVersion = "0.23.5"
val zioVersion    = "1.0.12"

val zioMagic = "io.github.kitlangton" %% "zio-magic" % "0.3.9"

val http4s = Seq(
  "org.http4s" %% "http4s-core"         % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-circe"        % http4sVersion,
  "org.http4s" %% "http4s-dsl"          % http4sVersion
)

val tests = Seq(
  "dev.zio" %% "zio-test"          % zioVersion,
  "dev.zio" %% "zio-test-sbt"      % zioVersion,
  "dev.zio" %% "zio-test-magnolia" % zioVersion,
  "dev.zio" %% "zio-interop-cats"  % "3.1.1.0"
)

val sttp = Seq(
  "com.softwaremill.sttp.client3" %% "core"                   % sttpVersion,
  "com.softwaremill.sttp.client3" %% "httpclient-backend-zio" % sttpVersion
)

// ### App Modules ###

/**
 * `root` is a "meta module". It's the "main module" of this project but doesn't have a physical existence. It represents the "current
 * project" if you prefer, composed of modules.
 *
 * The `aggregate` setting will instruct sbt that when you're launching an sbt command, you want it applied to all the aggregated modules
 */
lazy val root =
  Project(id = "test-compile", base = file("."))
    .aggregate(
      app,
      posts,
      potatoes,
      clients,
      calibanClients
    )
    .settings(
      // Additional scripted tests commands
      InputKey[Unit]("copy-file-with-options") := {
        val args: Vector[String] = spaceDelimited("<arg>").parsed.toVector

        IO.copy(
          List(file(args(3)) -> file(args(4))),
          overwrite = args(0).toBoolean,
          preserveLastModified = args(1).toBoolean,
          preserveExecutable = args(2).toBoolean
        )
      },
      InputKey[Unit]("sed-in-place") := {
        val args: Vector[String] = spaceDelimited("<arg>").parsed.toVector

        val previousValue = args(0)
        val newValue      = args(1)
        val baseDir       = baseDirectory.value.getAbsolutePath
        val initialFile   = s"$baseDir/${args(2)}"
        val backupFile    = s"$baseDir/${args(2)}.old"

        IO.move(file(initialFile), file(backupFile))
        val content    = IO.read(file(backupFile))
        val newContent = content.replace(previousValue, newValue)
        IO.write(file(initialFile), newContent)
      }
    )

lazy val app   =
  project
    .in(file("modules/app"))
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= Seq(zioMagic) ++ http4s ++ sttp.map(_ % Test))
    .dependsOn(posts, calibanClients % Test)

lazy val posts =
  project
    .in(file("modules/posts"))
    .settings(commonSettings: _*)
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings ++=
        Seq(
          "poc.caliban.posts.GraphQLApi.api" -> ClientGenerationSettings.default,
          "poc.caliban.posts.GraphQLApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.posts",
              clientName = "CalibanClient"
            ),
          "poc.caliban.posts.GraphQLApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.posts.split",
              splitFiles = true
            )
        )
    )
    .settings(libraryDependencies ++= calibanLib)

lazy val potatoes =
  project
    .in(file("modules/potatoes"))
    .settings(commonSettings: _*)
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanServerSettings :=
        Seq(
          "poc.caliban.potatoes.PotatoesApi.api" ->
            ClientGenerationSettings(
              packageName = "poc.caliban.client.generated.potatoes",
              splitFiles = true
            )
        )
    )
    .settings(libraryDependencies ++= calibanLib)

lazy val clients =
  project
    .in(file("modules/clients"))
    .settings(commonSettings: _*)
    .settings(libraryDependencies ++= sttp)
    .dependsOn(calibanClients)

lazy val calibanClients =
  project
    .withId("caliban-clients")
    .settings(commonSettings: _*)
    .in(file("modules/caliban-clients"))
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCalibanClient / ctCalibanClientsSettings := Seq(posts, potatoes)
    )
