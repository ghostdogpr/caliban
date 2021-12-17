import org.scalajs.linker.interface.ModuleSplitStyle
import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val scala212 = "2.12.14"
val scala213 = "2.13.7"
val scala3   = "3.1.0"
val allScala = Seq(scala212, scala213, scala3)

val akkaVersion             = "2.6.17"
val catsEffect2Version      = "2.5.4"
val catsEffect3Version      = "3.3.0"
val circeVersion            = "0.14.1"
val http4sVersion           = "0.23.7"
val laminextVersion         = "0.14.2"
val magnoliaVersion         = "0.17.0"
val mercatorVersion         = "0.2.1"
val playVersion             = "2.8.11"
val playJsonVersion         = "2.9.2"
val sttpVersion             = "3.3.18"
val tapirVersion            = "0.19.1"
val zioVersion              = "1.0.12"
val zio2Version             = "2.0.0-RC1"
val zioInteropCats2Version  = "2.5.1.0"
val zioInteropCats3Version  = "3.2.9.0"
val zio2InteropCats3Version = "3.3.0-RC1"
val zioConfigVersion        = "1.0.10"
val zqueryVersion           = "0.2.10"
val zquery2Version          = "0.3.0-RC1"
val zioJsonVersion          = "0.1.5"
val zioHttpVersion          = "1.0.0.0-RC17"

inThisBuild(
  List(
    scalaVersion             := scala212,
    crossScalaVersions       := allScala,
    organization             := "com.github.ghostdogpr",
    homepage                 := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses                 := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    Test / parallelExecution := false,
    scmInfo                  := Some(
      ScmInfo(
        url("https://github.com/ghostdogpr/caliban/"),
        "scm:git:git@github.com:ghostdogpr/caliban.git"
      )
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    ),
    developers               := List(
      Developer(
        "ghostdogpr",
        "Pierre Ricadat",
        "ghostdogpr@gmail.com",
        url("https://github.com/ghostdogpr")
      )
    ),
    ConsoleHelper.welcomeMessage
  )
)

name := "caliban"
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(publish / skip := true)
  .settings(crossScalaVersions := Nil)
  .aggregate(
    macros,
    core,
    http4s,
    akkaHttp,
    play,
    zioHttp,
    catsInterop,
    monixInterop,
    tapirInterop,
    clientJVM,
    clientJS,
    clientLaminext,
    tools,
    codegenSbt,
    federation
  )

lazy val macros = project
  .in(file("macros"))
  .settings(name := "caliban-macros")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) {
        Seq.empty
      } else {
        Seq(
          "com.propensive" %% "magnolia" % magnoliaVersion,
          "com.propensive" %% "mercator" % mercatorVersion
        )
      }
    }
  )

lazy val core = project
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) {
        Seq(
          "org.typelevel" %% "cats-parse" % "0.3.6"
        )
      } else {
        Seq(
          "com.propensive"    %% "magnolia"  % magnoliaVersion,
          "com.propensive"    %% "mercator"  % mercatorVersion,
          "com.lihaoyi"       %% "fastparse" % "2.3.3",
          "com.typesafe.play" %% "play-json" % playJsonVersion % Optional,
          "dev.zio"           %% "zio-json"  % zioJsonVersion  % Optional
        )
      }
    } ++
      Seq(
        "dev.zio"                     %% "zio"          % zio2Version,
        "dev.zio"                     %% "zio-streams"  % zio2Version,
        "dev.zio"                     %% "zio-query"    % zquery2Version,
        "dev.zio"                     %% "zio-test"     % zio2Version  % Test,
        "dev.zio"                     %% "zio-test-sbt" % zio2Version  % Test,
        "com.softwaremill.sttp.tapir" %% "tapir-core"   % tapirVersion % Optional,
        "io.circe"                    %% "circe-core"   % circeVersion % Optional,
        "io.circe"                    %% "circe-parser" % circeVersion % Test
      )
  )
  .dependsOn(macros)
  .settings(
    Test / fork := true,
    run / fork  := true
  )

lazy val tools = project
  .in(file("tools"))
  .settings(name := "caliban-tools")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "org.scalameta"                 %% "scalafmt-dynamic"              % "3.1.2",
      "org.scalameta"                 %% "scalafmt-core"                 % "3.1.2",
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
      "dev.zio"                       %% "zio-config"                    % zioConfigVersion,
      "dev.zio"                       %% "zio-config-magnolia"           % zioConfigVersion,
      "dev.zio"                       %% "zio-test"                      % zioVersion % Test,
      "dev.zio"                       %% "zio-test-sbt"                  % zioVersion % Test
    )
  )
  .dependsOn(core, clientJVM)

lazy val codegenSbt = project
  .in(file("codegen-sbt"))
  .settings(name := "caliban-codegen-sbt")
  .settings(commonSettings)
  .enablePlugins(BuildInfoPlugin)
  .settings(
    buildInfoKeys    := Seq[BuildInfoKey](version),
    buildInfoPackage := "caliban.codegen",
    buildInfoObject  := "BuildInfo"
  )
  .settings(
    sbtPlugin          := true,
    crossScalaVersions := Seq(scala212),
    testFrameworks     := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
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
  .settings(
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full)))
    } ++ Seq(
      "dev.zio"       %% "zio-interop-cats" % zio2InteropCats3Version,
      "org.typelevel" %% "cats-effect"      % catsEffect3Version
    )
  )
  .dependsOn(core)

lazy val monixInterop = project
  .in(file("interop/monix"))
  .settings(name := "caliban-monix")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"  %% "zio-interop-reactivestreams" % "1.3.8",
      "dev.zio"  %% "zio-interop-cats"            % zioInteropCats2Version,
      "io.monix" %% "monix"                       % "3.4.0"
    )
  )
  .dependsOn(core)

lazy val tapirInterop = project
  .in(file("interop/tapir"))
  .settings(name := "caliban-tapir")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full)))
    } ++
      Seq(
        "com.softwaremill.sttp.tapir"   %% "tapir-core"                    % tapirVersion,
        "com.softwaremill.sttp.tapir"   %% "tapir-zio"                     % tapirVersion,
        "com.softwaremill.sttp.tapir"   %% "tapir-sttp-client"             % tapirVersion % Test,
        "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"              % tapirVersion % Test,
        "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion  % Test,
        "dev.zio"                       %% "zio-test"                      % zioVersion   % Test,
        "dev.zio"                       %% "zio-test-sbt"                  % zioVersion   % Test
      )
  )
  .dependsOn(core)

lazy val http4s = project
  .in(file("adapters/http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= {
      if (scalaVersion.value == scala3) Seq()
      else Seq(compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.2").cross(CrossVersion.full)))
    } ++
      Seq(
        "dev.zio"                       %% "zio-interop-cats"              % zioInteropCats3Version,
        "org.typelevel"                 %% "cats-effect"                   % catsEffect3Version,
        "com.softwaremill.sttp.tapir"   %% "tapir-zio-http4s-server"       % tapirVersion,
        "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"              % tapirVersion,
        "org.http4s"                    %% "http4s-blaze-server"           % http4sVersion % Test,
        "dev.zio"                       %% "zio-test"                      % zioVersion    % Test,
        "dev.zio"                       %% "zio-test-sbt"                  % zioVersion    % Test,
        "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion   % Test,
        "com.softwaremill.sttp.client3" %% "circe"                         % sttpVersion   % Test,
        "io.circe"                      %% "circe-generic"                 % circeVersion  % Test
      )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test", catsInterop)

lazy val zioHttp = project
  .in(file("adapters/zio-http"))
  .settings(name := "caliban-zio-http")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "io.d11"                      %% "zhttp"                 % zioHttpVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe"      % tapirVersion
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val akkaHttp = project
  .in(file("adapters/akka-http"))
  .settings(name := "caliban-akka-http")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.typesafe.akka"             %% "akka-http"                  % "10.2.7",
      "com.typesafe.akka"             %% "akka-serialization-jackson" % akkaVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-akka-http-server"     % tapirVersion,
      compilerPlugin(("org.typelevel" %% "kind-projector"             % "0.13.2").cross(CrossVersion.full))
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val play = project
  .in(file("adapters/play"))
  .settings(name := "caliban-play")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.typesafe.play"             %% "play"                          % playVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-play-server"             % tapirVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-json-play"               % tapirVersion,
      "dev.zio"                       %% "zio-test"                      % zioVersion   % Test,
      "dev.zio"                       %% "zio-test-sbt"                  % zioVersion   % Test,
      "com.typesafe.play"             %% "play-akka-http-server"         % playVersion  % Test,
      "io.circe"                      %% "circe-generic"                 % circeVersion % Test,
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion  % Test,
      "com.softwaremill.sttp.client3" %% "circe"                         % sttpVersion  % Test,
      compilerPlugin(("org.typelevel" %% "kind-projector"                % "0.13.2").cross(CrossVersion.full))
    )
  )
  .dependsOn(core, tapirInterop % "compile->compile;test->test")

lazy val client    = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("client"))
  .settings(name := "caliban-client")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "io.circe"                      %%% "circe-parser" % circeVersion,
      "com.softwaremill.sttp.client3" %%% "core"         % sttpVersion,
      "com.softwaremill.sttp.client3" %%% "circe"        % sttpVersion,
      "dev.zio"                       %%% "zio-test"     % zio2Version % Test,
      "dev.zio"                       %%% "zio-test-sbt" % zio2Version % Test
    )
  )
lazy val clientJVM = client.jvm
lazy val clientJS  = client.js
  .settings(
    libraryDependencies ++= {
      Seq("io.github.cquiroz" %%% "scala-java-time" % "2.3.0" % Test)
    }
  )
  .settings(scalaVersion := scala213)
  .settings(crossScalaVersions := allScala)

lazy val clientLaminext = crossProject(JSPlatform)
  .crossType(CrossType.Pure)
  .js
  .in(file("client-laminext"))
  .settings(scalaVersion := scala213)
  .settings(crossScalaVersions := Seq(scala213, scala3))
  .settings(name := "caliban-client-laminext")
  .settings(commonSettings)
  .dependsOn(clientJS)
  .settings(
    testFrameworks                         := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    Test / scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.FewestModules) },
    Test / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := true,
    Test / scalaJSUseTestModuleInitializer := false,
    libraryDependencies ++= Seq(
      "io.laminext" %%% "core"            % laminextVersion,
      "io.laminext" %%% "fetch"           % laminextVersion,
      "io.laminext" %%% "fetch-circe"     % laminextVersion,
      "io.laminext" %%% "websocket"       % laminextVersion,
      "io.laminext" %%% "websocket-circe" % laminextVersion,
      "dev.zio"     %%% "zio-test"        % zio2Version % Test,
      "dev.zio"     %%% "zio-test-sbt"    % zio2Version % Test
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
    crossScalaVersions -= scala3,
    libraryDependencySchemes += "org.scala-lang.modules" %% "scala-java8-compat" % "always",
    libraryDependencies ++= Seq(
      "org.http4s"                    %% "http4s-blaze-server"           % http4sVersion,
      "org.http4s"                    %% "http4s-dsl"                    % http4sVersion,
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
      "io.circe"                      %% "circe-generic"                 % circeVersion,
      "io.d11"                        %% "zhttp"                         % zioHttpVersion,
      "com.typesafe.play"             %% "play-akka-http-server"         % playVersion,
      "com.typesafe.akka"             %% "akka-actor-typed"              % akkaVersion
    )
  )
  .dependsOn(
    akkaHttp,
    http4s,
    catsInterop,
    play,
    /*monixInterop,*/
    tapirInterop,
    clientJVM,
    federation,
    zioHttp,
    tools
  )

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "org.sangria-graphql" %% "sangria"       % "2.1.3",
      "org.sangria-graphql" %% "sangria-circe" % "1.3.0"
    )
  )

lazy val federation = project
  .in(file("federation"))
  .settings(name := "caliban-federation")
  .settings(commonSettings)
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    testFrameworks       := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zio2Version,
      "dev.zio" %% "zio-test"     % zio2Version % Test,
      "dev.zio" %% "zio-test-sbt" % zio2Version % Test
    ),
    Compile / PB.targets := Seq(
      scalapb.gen(grpc = false) -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
    )
  )

lazy val commonSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    "-unchecked",
    "-Xfatal-warnings"
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
        "-opt-inline-from:<source>",
        "-opt-warnings",
        "-opt:l:inline",
        "-explaintypes"
      )
    case Some((2, 13)) =>
      Seq(
        "-Xlint:-byname-implicit",
        "-explaintypes"
      )

    case Some((3, _)) =>
      Seq(
        "-explain-types",
        "-Ykind-projector"
      )
    case _            => Nil
  })
)
