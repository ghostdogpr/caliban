import com.typesafe.tools.mima.core.{ DirectMissingMethodProblem, MissingClassProblem, ProblemFilters }
import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val scala212 = "2.12.19"
val scala213 = "2.13.14"
val scala3   = "3.3.3"
val allScala = Seq(scala212, scala213, scala3)

val akkaVersion               = "2.6.20"
val catsEffect3Version        = "3.5.4"
val catsMtlVersion            = "1.3.0"
val circeVersion              = "0.14.7"
val fs2Version                = "3.10.2"
val http4sVersion             = "0.23.27"
val javaTimeVersion           = "2.5.0"
val jsoniterVersion           = "2.28.5"
val laminextVersion           = "0.17.0"
val magnoliaScala2Version     = "1.1.9"
val magnoliaScala3Version     = "1.3.6"
val pekkoHttpVersion          = "1.0.1"
val playVersion               = "3.0.3"
val playJsonVersion           = "3.0.3"
val scalafmtVersion           = "3.8.0"
val sttpVersion               = "3.9.6"
val tapirVersion              = "1.10.7"
val zioVersion                = "2.1.1"
val zioInteropCats2Version    = "22.0.0.0"
val zioInteropCats3Version    = "23.1.0.2"
val zioInteropReactiveVersion = "2.0.2"
val zioConfigVersion          = "3.0.7"
val zqueryVersion             = "0.7.1"
val zioJsonVersion            = "0.6.2"
val zioHttpVersion            = "3.0.0-RC6+44-68b7cadb-SNAPSHOT"
val zioOpenTelemetryVersion   = "3.0.0-RC21"
val zioPreludeVersion         = "1.0.0-RC25"

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    scalaVersion             := scala213,
    crossScalaVersions       := allScala,
    organization             := "com.github.ghostdogpr",
    homepage                 := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses                 := List(License.Apache2),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    Test / parallelExecution := false,
    scmInfo                  := Some(
      ScmInfo(
        url("https://github.com/ghostdogpr/caliban/"),
        "scm:git:git@github.com:ghostdogpr/caliban.git"
      )
    ),
    developers               := List(
      Developer(
        "ghostdogpr",
        "Pierre Ricadat",
        "ghostdogpr@gmail.com",
        url("https://github.com/ghostdogpr")
      )
    ),
    versionScheme            := Some("pvp"),
    ConsoleHelper.welcomeMessage(scala212, scala213, scala3),
    // See https://github.com/playframework/playframework/issues/11461#issuecomment-1276028512
    // Can be removed when the entire Scala ecosystem has migrated to Scala 2.12.17+, sbt 1.8.x, and moved away from scala-xml v1 in general.
    libraryDependencySchemes ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
    )
  )
)

name := "caliban"
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val allProjects: Seq[ProjectReference] =
  List(
    macros,
    core,
    http4s,
    akkaHttp,
    pekkoHttp,
    play,
    zioHttp,
    quickAdapter,
    catsInterop,
    monixInterop,
    tapirInterop,
    clientJVM,
    clientJS,
    clientNative,
    clientLaminext,
    tools,
    codegenSbt,
    federation,
    reporting,
    tracing
  )

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(publish / skip := true)
  .settings(crossScalaVersions := Nil)
  .aggregate(allProjects *)

lazy val rootJVM212 = project
  .in(file("target/rootJVM212"))
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true,
    ideSkipProject     := true
  )
  .aggregate({
    val excluded: Set[ProjectReference] = Set(clientJS, clientNative, clientLaminext, play)
    allProjects.filterNot(excluded.contains)
  } *)

lazy val rootJVM213 = project
  .in(file("target/rootJVM213"))
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true,
    ideSkipProject     := true
  )
  .aggregate({
    val excluded: Set[ProjectReference] = Set(clientJS, clientNative, clientLaminext, codegenSbt)
    allProjects.filterNot(excluded.contains)
  } *)

lazy val rootJVM3 = project
  .in(file("target/rootJVM3"))
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true,
    ideSkipProject     := true
  )
  .aggregate({
    val excluded: Set[ProjectReference] = Set(clientJS, clientNative, clientLaminext, codegenSbt, akkaHttp)
    allProjects.filterNot(excluded.contains)
  } *)

lazy val macros = project
  .in(file("macros"))
  .settings(name := "caliban-macros")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) {
        Seq(
          "com.softwaremill.magnolia1_3" %% "magnolia" % magnoliaScala3Version
        )
      } else {
        Seq(
          "com.softwaremill.magnolia1_2" %% "magnolia"      % magnoliaScala2Version,
          "org.scala-lang"                % "scala-reflect" % scalaVersion.value
        )
      }
    }
  )

lazy val core = project
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++=
      Seq(
        "com.lihaoyi"                           %% "fastparse"               % "3.1.0",
        "org.scala-lang.modules"                %% "scala-collection-compat" % "2.12.0",
        "dev.zio"                               %% "zio"                     % zioVersion,
        "dev.zio"                               %% "zio-streams"             % zioVersion,
        "dev.zio"                               %% "zio-query"               % zqueryVersion,
        "dev.zio"                               %% "zio-prelude"             % zioPreludeVersion,
        "dev.zio"                               %% "zio-test"                % zioVersion      % Test,
        "dev.zio"                               %% "zio-test-sbt"            % zioVersion      % Test,
        "dev.zio"                               %% "zio-json"                % zioJsonVersion  % Optional,
        "com.softwaremill.sttp.tapir"           %% "tapir-core"              % tapirVersion    % Optional,
        "io.circe"                              %% "circe-core"              % circeVersion    % Optional,
        "io.circe"                              %% "circe-parser"            % circeVersion    % Test,
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"     % jsoniterVersion % Optional,
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"   % jsoniterVersion % Provided,
        "org.playframework"                     %% "play-json"               % playJsonVersion % Optional,
        "org.apache.commons"                     % "commons-lang3"           % "3.14.0"        % Test
      )
  )
  .dependsOn(macros)
  .settings(
    Test / fork := true,
    run / fork  := true
  )

lazy val tools = project
  .in(file("tools"))
  .enablePlugins(BuildInfoPlugin)
  .settings(name := "caliban-tools")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](
      "scalaPartialVersion" -> CrossVersion.partialVersion(scalaVersion.value),
      "scalafmtVersion"     -> scalafmtVersion
    ),
    buildInfoPackage := "caliban.tools",
    buildInfoObject  := "BuildInfo"
  )
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "org.scalameta"                  % "scalafmt-interfaces" % scalafmtVersion,
      "io.get-coursier"                % "interface"           % "1.0.19",
      "com.softwaremill.sttp.client3" %% "zio"                 % sttpVersion,
      "dev.zio"                       %% "zio-test"            % zioVersion     % Test,
      "dev.zio"                       %% "zio-test-sbt"        % zioVersion     % Test,
      "dev.zio"                       %% "zio-json"            % zioJsonVersion % Test
    )
  )
  .dependsOn(core, clientJVM)

lazy val tracing = project
  .in(file("tracing"))
  .enablePlugins(BuildInfoPlugin)
  .settings(name := "caliban-tracing")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    buildInfoPackage := "caliban.tracing",
    buildInfoObject  := "BuildInfo"
  )
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio"         %% "zio-opentelemetry"         % zioOpenTelemetryVersion,
      "dev.zio"         %% "zio-test"                  % zioVersion % Test,
      "dev.zio"         %% "zio-test-sbt"              % zioVersion % Test,
      "io.opentelemetry" % "opentelemetry-sdk-testing" % "1.38.0"   % Test
    )
  )
  .dependsOn(core, tools)

lazy val codegenSbt = project
  .in(file("codegen-sbt"))
  .settings(name := "caliban-codegen-sbt")
  .settings(commonSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    skip             := (scalaVersion.value != scala212),
    ideSkipProject   := (scalaVersion.value != scala212),
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "caliban.codegen",
    buildInfoObject  := "BuildInfo"
  )
  .settings(
    sbtPlugin          := true,
    crossScalaVersions := Seq(scala212),
    testFrameworks     := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-config"          % zioConfigVersion,
      "dev.zio" %% "zio-config-magnolia" % zioConfigVersion,
      "dev.zio" %% "zio-test-sbt"        % zioVersion % Test
    )
  )
  .enablePlugins(SbtPlugin)
  .settings(
    scriptedLaunchOpts   := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Xss4M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog    := false,
    scriptedDependencies := {
      (macros / publishLocal).value
      (core / publishLocal).value
      (clientJVM / publishLocal).value
      (tools / publishLocal).value
      publishLocal.value
    }
  )
  .dependsOn(tools)

lazy val catsInterop = project
  .in(file("interop/cats"))
  .settings(name := "caliban-cats")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)))
    } ++ Seq(
      "org.typelevel" %% "cats-effect"      % catsEffect3Version,
      "co.fs2"        %% "fs2-core"         % fs2Version,
      "dev.zio"       %% "zio-interop-cats" % zioInteropCats3Version,
      "dev.zio"       %% "zio-test"         % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt"     % zioVersion % Test
    )
  )
  .dependsOn(core)

lazy val monixInterop = project
  .in(file("interop/monix"))
  .settings(name := "caliban-monix")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"  %% "zio-interop-reactivestreams" % zioInteropReactiveVersion,
      "dev.zio"  %% "zio-interop-cats"            % zioInteropCats2Version,
      "io.monix" %% "monix"                       % "3.4.1"
    )
  )
  .dependsOn(core)

lazy val tapirInterop = project
  .in(file("interop/tapir"))
  .settings(name := "caliban-tapir")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)))
    } ++
      Seq(
        "com.softwaremill.sttp.tapir"   %% "tapir-core"                    % tapirVersion,
        "com.softwaremill.sttp.tapir"   %% "tapir-zio"                     % tapirVersion,
        "com.softwaremill.sttp.tapir"   %% "tapir-sttp-client"             % tapirVersion   % Test,
        "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion    % Test,
        "dev.zio"                       %% "zio-json"                      % zioJsonVersion % Test,
        "dev.zio"                       %% "zio-test"                      % zioVersion     % Test,
        "dev.zio"                       %% "zio-test-sbt"                  % zioVersion     % Test
      )
  )
  .dependsOn(core)

lazy val http4s = project
  .in(file("adapters/http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)))
    } ++
      Seq(
        "dev.zio"                               %% "zio-interop-cats"        % zioInteropCats3Version,
        "org.typelevel"                         %% "cats-effect"             % catsEffect3Version,
        "com.softwaremill.sttp.tapir"           %% "tapir-http4s-server-zio" % tapirVersion,
        "com.softwaremill.sttp.tapir"           %% "tapir-json-circe"        % tapirVersion    % Test,
        "com.softwaremill.sttp.tapir"           %% "tapir-jsoniter-scala"    % tapirVersion    % Test,
        "org.http4s"                            %% "http4s-ember-server"     % http4sVersion   % Test,
        "dev.zio"                               %% "zio-test"                % zioVersion      % Test,
        "dev.zio"                               %% "zio-test-sbt"            % zioVersion      % Test,
        "com.softwaremill.sttp.client3"         %% "circe"                   % sttpVersion     % Test,
        "io.circe"                              %% "circe-generic"           % circeVersion    % Test,
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"   % jsoniterVersion % Test
      )
  )
  .dependsOn(core % "compile->compile;test->test", tapirInterop % "compile->compile;test->test", catsInterop)

lazy val zioHttp = project
  .in(file("adapters/zio-http"))
  .settings(name := "caliban-zio-http")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio"                     %% "zio-http"              % zioHttpVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirVersion,
      "dev.zio"                     %% "zio-json"              % zioJsonVersion % Test,
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio"        % tapirVersion   % Test
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val quickAdapter = project
  .in(file("adapters/quick"))
  .settings(name := "caliban-quick")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio"                               %% "zio-http"              % zioHttpVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % Provided,
      "com.softwaremill.sttp.tapir"           %% "tapir-jsoniter-scala"  % tapirVersion    % Test
    )
  )
  .dependsOn(core, tapirInterop % "test->test")

lazy val akkaHttp = project
  .in(file("adapters/akka-http"))
  .settings(name := "caliban-akka-http")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    skip           := (scalaVersion.value == scala3),
    ideSkipProject := (scalaVersion.value == scala3),
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.typesafe.akka"             %% "akka-http"                  % "10.2.10",
      "com.typesafe.akka"             %% "akka-serialization-jackson" % akkaVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-akka-http-server"     % tapirVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"           % tapirVersion % Test,
      compilerPlugin(("org.typelevel" %% "kind-projector"             % "0.13.3").cross(CrossVersion.full))
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val pekkoHttp = project
  .in(file("adapters/pekko-http"))
  .settings(name := "caliban-pekko-http")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)))
    } ++ Seq(
      "org.apache.pekko"            %% "pekko-http"              % pekkoHttpVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-pekko-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe"        % tapirVersion % Test
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val play = project
  .in(file("adapters/play"))
  .settings(name := "caliban-play")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .settings(
    skip           := (scalaVersion.value == scala212),
    ideSkipProject := (scalaVersion.value == scala212),
    crossScalaVersions -= scala212,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.3").cross(CrossVersion.full)))
    },
    libraryDependencies ++= Seq(
      "org.playframework"           %% "play"                   % playVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-play-server"      % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-play"        % tapirVersion    % Test,
      "dev.zio"                     %% "zio-test"               % zioVersion      % Test,
      "dev.zio"                     %% "zio-test-sbt"           % zioVersion      % Test,
      "org.playframework"           %% "play-pekko-http-server" % playVersion     % Test,
      "org.playframework"           %% "play-json"              % playJsonVersion % Test
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val client    = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("client"))
  .settings(name := "caliban-client")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3"        %%% "core"                  % sttpVersion,
      "com.softwaremill.sttp.client3"        %%% "jsoniter"              % sttpVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % Provided,
      "dev.zio"                              %%% "zio-test"              % zioVersion      % Test,
      "dev.zio"                              %%% "zio-test-sbt"          % zioVersion      % Test
    )
  )
lazy val clientJVM = client.jvm.settings(enableMimaSettingsJVM)
lazy val clientJS  = client.js
  .settings(enableMimaSettingsJS)
  .settings(
    libraryDependencies ++= {
      Seq(
        "org.scala-js"      %%% "scalajs-java-securerandom" % "1.0.0" cross CrossVersion.for3Use2_13,
        "io.github.cquiroz" %%% "scala-java-time"           % "2.5.0" % Test
      )
    }
  )
  .settings(scalaVersion := scala213)
  .settings(crossScalaVersions := allScala)

lazy val clientNative = client.native
  .settings(
    libraryDependencies ++= Seq(
      "com.github.lolgab" %%% "scala-native-crypto" % "0.0.4",
      "io.github.cquiroz" %%% "scala-java-time"     % javaTimeVersion % Test
    ),
    Test / fork := false
  )

lazy val clientLaminext = crossProject(JSPlatform)
  .crossType(CrossType.Pure)
  .js
  .in(file("client-laminext"))
  .settings(scalaVersion := scala213)
  .settings(crossScalaVersions := Seq(scala213, scala3))
  .settings(name := "caliban-client-laminext")
  .settings(commonSettings)
  .settings(enableMimaSettingsJS)
  .dependsOn(clientJS)
  .settings(
    testFrameworks                         := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.FewestModules) },
    Test / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseTestModuleInitializer := false,
    libraryDependencies ++= Seq(
      "io.laminext"                           %%% "core"                % laminextVersion,
      "io.laminext"                           %%% "fetch"               % laminextVersion,
      "io.laminext"                           %%% "websocket"           % laminextVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterVersion,
      "dev.zio"                               %%% "zio-test"            % zioVersion % Test,
      "dev.zio"                               %%% "zio-test-sbt"        % zioVersion % Test
    )
  )

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(
    publish / skip     := true,
    run / fork         := true,
    run / connectInput := true
  )
  .settings(
    skip                                                 := (scalaVersion.value != scala213),
    ideSkipProject                                       := (scalaVersion.value != scala213),
    crossScalaVersions                                   := Seq(scala213),
    libraryDependencySchemes += "org.scala-lang.modules" %% "scala-java8-compat" % "always",
    libraryDependencies ++= Seq(
      "org.typelevel"                         %% "cats-mtl"               % catsMtlVersion,
      "org.http4s"                            %% "http4s-ember-server"    % http4sVersion,
      "org.http4s"                            %% "http4s-dsl"             % http4sVersion,
      "com.softwaremill.sttp.client3"         %% "zio"                    % sttpVersion,
      "io.circe"                              %% "circe-generic"          % circeVersion,
      "dev.zio"                               %% "zio-http"               % zioHttpVersion,
      "org.playframework"                     %% "play-pekko-http-server" % playVersion,
      "com.typesafe.akka"                     %% "akka-actor-typed"       % akkaVersion,
      "com.softwaremill.sttp.tapir"           %% "tapir-jsoniter-scala"   % tapirVersion,
      "com.softwaremill.sttp.tapir"           %% "tapir-json-circe"       % tapirVersion,
      "com.softwaremill.sttp.tapir"           %% "tapir-json-play"        % tapirVersion,
      "com.softwaremill.sttp.tapir"           %% "tapir-jsoniter-scala"   % tapirVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros"  % jsoniterVersion % Provided
    )
  )
  .dependsOn(
    akkaHttp,
    pekkoHttp,
    http4s,
    catsInterop,
    quickAdapter,
    play,
    /*monixInterop,*/
    tapirInterop,
    clientJVM,
    federation,
    zioHttp,
    tools
  )

lazy val reporting = project
  .in(file("reporting"))
  .settings(name := "caliban-reporting")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .dependsOn(clientJVM, core)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio"                       %% "zio"          % zioVersion,
      "com.softwaremill.sttp.client3" %% "core"         % sttpVersion,
      "dev.zio"                       %% "zio-test"     % zioVersion % Test,
      "dev.zio"                       %% "zio-test-sbt" % zioVersion % Test
    )
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(
    skip               := (scalaVersion.value == scala212),
    ideSkipProject     := (scalaVersion.value == scala212),
    publish / skip     := true,
    crossScalaVersions := Seq(scala213, scala3)
  )
  .dependsOn(core % "compile->compile")
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.sangria-graphql"                   %% "sangria"             % "4.0.1",
      "org.sangria-graphql"                   %% "sangria-circe"       % "1.3.2",
      "edu.gemini"                            %% "gsp-graphql-core"    % "0.13.0",
      "edu.gemini"                            %% "gsp-graphql-generic" % "0.13.0",
      "io.github.valdemargr"                  %% "gql-server"          % "0.3.3",
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % jsoniterVersion,
      "io.circe"                              %% "circe-parser"        % circeVersion,
      "dev.zio"                               %% "zio-json"            % zioJsonVersion
    )
  )

lazy val federation = project
  .in(file("federation"))
  .settings(name := "caliban-federation")
  .settings(commonSettings)
  .settings(enableMimaSettingsJVM)
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    testFrameworks       := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    Compile / PB.targets := Seq(
      scalapb.gen(grpc = false) -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
    )
  )

lazy val docs = project
  .in(file("mdoc"))
  .enablePlugins(MdocPlugin)
  .settings(commonSettings)
  .settings(
    skip               := (scalaVersion.value == scala3),
    ideSkipProject     := (scalaVersion.value == scala3),
    crossScalaVersions := Seq(scala212, scala213),
    name               := "caliban-docs",
    mdocIn             := (ThisBuild / baseDirectory).value / "vuepress" / "docs",
    run / fork         := true,
    scalacOptions -= "-Xfatal-warnings",
    scalacOptions += "-Wunused:imports",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3" %% "zio"              % sttpVersion,
      "io.circe"                      %% "circe-generic"    % circeVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-json-circe" % tapirVersion,
      "org.typelevel"                 %% "cats-mtl"         % catsMtlVersion
    )
  )
  .dependsOn(core, catsInterop, tapirInterop, http4s, tools, quickAdapter)

lazy val commonSettings = Def.settings(
  apiMappingSettings,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    "-unchecked",
    "-Xfatal-warnings",
    "-release",
    "11"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Seq(
        "-Xsource:2.13",
        "-Yno-adapted-args",
        "-Ypartial-unification",
        "-Ywarn-extra-implicit",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-unused:-nowarn",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-opt-warnings",
        "-opt:l:method",
        "-opt:l:inline",
        "-opt-inline-from:scala.**",
        "-explaintypes"
      )
    case Some((2, 13)) =>
      Seq(
        "-Xlint:-byname-implicit",
        "-Ybackend-parallelism:4",
        "-opt:l:method",
        "-opt:l:inline",
        "-opt-inline-from:scala.**",
        "-explaintypes"
      )

    case Some((3, _)) =>
      Seq(
        "-explain-types",
        "-Ykind-projector",
        "-no-indent"
      )
    case _            => Nil
  })
)

lazy val enforceMimaCompatibility = false // Enable / disable failing CI on binary incompatibilities

lazy val enableMimaSettingsJVM =
  Def.settings(
    mimaFailOnProblem     := enforceMimaCompatibility,
    mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %% moduleName.value % _).toSet,
    mimaBinaryIssueFilters ++= Seq()
  )

lazy val enableMimaSettingsJS =
  Def.settings(
    mimaFailOnProblem     := enforceMimaCompatibility,
    mimaPreviousArtifacts := previousStableVersion.value.map(organization.value %%% moduleName.value % _).toSet,
    mimaBinaryIssueFilters ++= Seq()
  )

lazy val apiMappingSettings = Def.settings(
  autoAPIMappings := true,
  apiMappings ++= {
    val depsByModule = (Compile / dependencyClasspathAsJars).value.flatMap { dep =>
      dep.get(moduleID.key).map((_, dep.data))
    }.groupBy { case (moduleID, _) => (moduleID.organization, moduleID.name) }.mapValues(_.head)

    val cross = CrossVersion(crossVersion.value, scalaVersion.value, scalaBinaryVersion.value)
      .getOrElse((s: String) => s)

    def depFile(org: String, name: String) = depsByModule.get((org, cross(name)))

    def javadocIOUrl(id: ModuleID) = url(s"https://javadoc.io/doc/${id.organization}/${id.name}/${id.revision}/")

    def javadocIO(org: String, name: String) = depFile(org, name).map { case (id, f) => f -> javadocIOUrl(id) }

    Seq(
      javadocIO("dev.zio", "zio"),
      javadocIO("dev.zio", "zio-query")
    ).flatten.toMap
  }
)

Global / excludeLintKeys += ideSkipProject
