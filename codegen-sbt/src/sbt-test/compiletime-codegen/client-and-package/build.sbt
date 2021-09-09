import Libraries._
import sbt.librarymanagement.Resolver

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "io.guizmaii.poc"
ThisBuild / homepage := Some(url("https://www.conduktor.io/"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "2.12.13" // Must stay 2.12 in these tests because the plugin is compiled with 2.12
ThisBuild / resolvers += Resolver.mavenLocal

// ### App Modules ###

/**
 * `root` is a "meta module". It's the "main module" of this project but doesn't have a physical existence. It represents the "current
 * project" if you prefer, composed of modules.
 *
 * The `aggregate` setting will instruct sbt that when you're launching an sbt command, you want it applied to all the aggregated modules
 */
lazy val root =
  Project(id = "poc_compile_time_caliban_client_generation", base = file("."))
    .aggregate(
      server,
      client,
      calibanClient
    )

lazy val server =
  project
    .in(file("modules/server"))
    .enablePlugins(CompileTimeCalibanServerPlugin)
    .settings(
      Compile / ctCalibanServer / ctCalibanApiRefs := Seq("io.guizmaii.poc.caliban.server.GraphQLApi.api")
    )
    .settings(libraryDependencies ++= calibanLibs)

lazy val client =
  project
    .in(file("modules/client"))
    .settings(libraryDependencies ++= sttp)
    .dependsOn(calibanClient)

lazy val calibanClient =
  project
    .withId("caliban-client")
    .in(file("modules/caliban-client"))
    .enablePlugins(CompileTimeCalibanClientPlugin)
    .settings(
      Compile / ctCaliban / ctCalibanSettings := server -> GenerateClientSettings(
        clientName = "CalibanClient",
        packageName = "io.guizmaii.poc.caliban.client.generated"
      )
    )
