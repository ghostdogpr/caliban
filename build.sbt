import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val scala212 = "2.12.13"
val scala213 = "2.13.5"
val scala3   = "3.0.0-RC3"
val allScala = Seq(scala212, scala213, scala3)

val akkaVersion           = "2.6.14"
val catsEffectVersion     = "2.5.1"
val circeVersion          = "0.14.0-M6"
val http4sVersion         = "0.21.23"
val magnoliaVersion       = "0.17.0"
val mercatorVersion       = "0.2.1"
val playVersion           = "2.8.8"
val playJsonVersion       = "2.9.2"
val silencerVersion       = "1.7.3"
val sttpVersion           = "3.3.2"
val tapirVersion          = "0.17.18"
val zioVersion            = "1.0.7"
val zioInteropCatsVersion = "2.4.1.0"
val zioConfigVersion      = "1.0.5"
val zqueryVersion         = "0.2.8"
val zioJsonVersion        = "0.1.4"
val zioHttpVersion        = "1.0.0.0-RC16"

inThisBuild(
  List(
    scalaVersion := scala212,
    crossScalaVersions := allScala,
    organization := "com.github.ghostdogpr",
    homepage := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    Test / parallelExecution := false,
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/ghostdogpr/caliban/"),
        "scm:git:git@github.com:ghostdogpr/caliban.git"
      )
    ),
    developers := List(
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
    finch,
    http4s,
    akkaHttp,
    play,
    zioHttp,
    catsInterop,
    monixInterop,
    tapirInterop,
    clientJVM,
    clientJS,
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
          "org.typelevel" %% "cats-parse" % "0.3.3"
        )
      } else {
        Seq(
          "com.propensive"    %% "magnolia"  % magnoliaVersion,
          "com.propensive"    %% "mercator"  % mercatorVersion,
          "com.lihaoyi"       %% "fastparse" % "2.3.2",
          "com.typesafe.play" %% "play-json" % playJsonVersion % Optional,
          "dev.zio"           %% "zio-json"  % zioJsonVersion  % Optional
        )
      }
    } ++
      Seq(
        "dev.zio"  %% "zio"          % zioVersion,
        "dev.zio"  %% "zio-streams"  % zioVersion,
        "dev.zio"  %% "zio-query"    % zqueryVersion,
        "dev.zio"  %% "zio-test"     % zioVersion   % "test",
        "dev.zio"  %% "zio-test-sbt" % zioVersion   % "test",
        "io.circe" %% "circe-core"   % circeVersion % Optional
      )
  )
  .dependsOn(macros)
  .settings(
    Test / fork := true,
    run / fork := true
  )

lazy val tools = project
  .in(file("tools"))
  .settings(name := "caliban-tools")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "org.scalameta"                 %% "scalafmt-dynamic"              % "2.7.5",
      "org.scalameta"                 %% "scalafmt-core"                 % "2.7.5",
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
      "dev.zio"                       %% "zio-config"                    % zioConfigVersion,
      "dev.zio"                       %% "zio-config-magnolia"           % zioConfigVersion,
      "dev.zio"                       %% "zio-test"                      % zioVersion % "test",
      "dev.zio"                       %% "zio-test-sbt"                  % zioVersion % "test"
    )
  )
  .dependsOn(core, clientJVM)

lazy val codegenSbt = project
  .in(file("codegen-sbt"))
  .settings(name := "caliban-codegen-sbt")
  .settings(commonSettings)
  .settings(
    sbtPlugin := true,
    crossScalaVersions := Seq(scala212),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .enablePlugins(SbtPlugin)
  .settings(
    scriptedLaunchOpts := {
      scriptedLaunchOpts.value ++
        Seq("-Xmx1024M", "-Dplugin.version=" + version.value)
    },
    scriptedBufferLog := false,
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
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-interop-cats" % zioInteropCatsVersion,
      "org.typelevel" %% "cats-effect"      % catsEffectVersion
    )
  )
  .dependsOn(core)

lazy val monixInterop = project
  .in(file("interop/monix"))
  .settings(name := "caliban-monix")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "dev.zio"  %% "zio-interop-reactivestreams" % "1.0.3.5-RC12",
      "dev.zio"  %% "zio-interop-cats"            % zioInteropCatsVersion,
      "io.monix" %% "monix"                       % "3.4.0"
    )
  )
  .dependsOn(core)

lazy val tapirInterop = project
  .in(file("interop/tapir"))
  .settings(name := "caliban-tapir")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir"   %% "tapir-core"     % tapirVersion,
      "dev.zio"                       %% "zio-test"       % zioVersion % "test",
      "dev.zio"                       %% "zio-test-sbt"   % zioVersion % "test",
      compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.0").cross(CrossVersion.full))
    )
  )
  .dependsOn(core)

lazy val http4s = project
  .in(file("adapters/http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "dev.zio"                       %% "zio-interop-cats"    % zioInteropCatsVersion,
      "org.typelevel"                 %% "cats-effect"         % catsEffectVersion,
      "org.http4s"                    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"                    %% "http4s-circe"        % http4sVersion,
      "org.http4s"                    %% "http4s-blaze-server" % http4sVersion,
      "io.circe"                      %% "circe-parser"        % circeVersion,
      compilerPlugin(("org.typelevel" %% "kind-projector"      % "0.13.0").cross(CrossVersion.full)),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
      "com.github.ghik"                % "silencer-lib"        % silencerVersion % Provided cross CrossVersion.full
    )
  )
  .dependsOn(core)

lazy val zioHttp = project
  .in(file("adapters/zio-http"))
  .settings(name := "caliban-zio-http")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.d11"   %% "zhttp"         % zioHttpVersion,
      "io.circe" %% "circe-parser"  % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion
    )
  )
  .dependsOn(core)

lazy val akkaHttp = project
  .in(file("adapters/akka-http"))
  .settings(name := "caliban-akka-http")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "com.typesafe.akka"             %% "akka-http"                  % "10.2.4",
      "com.typesafe.akka"             %% "akka-serialization-jackson" % akkaVersion,
      "com.typesafe.akka"             %% "akka-stream"                % akkaVersion,
      "de.heikoseeberger"             %% "akka-http-circe"            % "1.36.0" % Optional,
      "de.heikoseeberger"             %% "akka-http-play-json"        % "1.36.0" % Optional,
      compilerPlugin(("org.typelevel" %% "kind-projector"             % "0.13.0").cross(CrossVersion.full))
    )
  )
  .dependsOn(core)

lazy val finch = project
  .in(file("adapters/finch"))
  .settings(name := "caliban-finch")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core"      % "0.32.1",
      "com.github.finagle" %% "finchx-circe"     % "0.32.1",
      "dev.zio"            %% "zio-interop-cats" % zioInteropCatsVersion,
      "org.typelevel"      %% "cats-effect"      % catsEffectVersion,
      "io.circe"           %% "circe-parser"     % circeVersion
    )
  )
  .dependsOn(core)

lazy val play = project
  .in(file("adapters/play"))
  .settings(name := "caliban-play")
  .settings(commonSettings)
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.typesafe.play"             %% "play"                          % playVersion,
      "dev.zio"                       %% "zio-test"                      % zioVersion   % "test",
      "dev.zio"                       %% "zio-test-sbt"                  % zioVersion   % "test",
      "com.typesafe.play"             %% "play-akka-http-server"         % playVersion  % "test",
      "io.circe"                      %% "circe-generic"                 % circeVersion % "test",
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion  % "test",
      "com.softwaremill.sttp.client3" %% "circe"                         % sttpVersion  % "test"
    )
  )
  .dependsOn(core)

lazy val client    = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("client"))
  .settings(name := "caliban-client")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "io.circe"                      %%% "circe-core" % circeVersion,
      "com.softwaremill.sttp.client3" %%% "core"       % sttpVersion,
      "com.softwaremill.sttp.client3" %%% "circe"      % sttpVersion
    )
  )
lazy val clientJVM = client.jvm.settings(
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio-test"     % zioVersion % "test",
    "dev.zio" %%% "zio-test-sbt" % zioVersion % "test"
  )
)
lazy val clientJS  = client.js.settings(
  libraryDependencies ++= {
    // ZIO test is not published for Scala 3 on Scala.js yet
    if (scalaVersion.value == scala3) {
      Seq.empty
    } else {
      Seq(
        "dev.zio"           %%% "zio-test"        % zioVersion % "test",
        "dev.zio"           %%% "zio-test-sbt"    % zioVersion % "test",
        "io.github.cquiroz" %%% "scala-java-time" % "2.2.2"    % Test
      )
    }
  }
)

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "de.heikoseeberger"             %% "akka-http-circe"               % "1.36.0",
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"              % tapirVersion,
      "io.circe"                      %% "circe-generic"                 % circeVersion,
      "io.d11"                        %% "zhttp"                         % zioHttpVersion,
      "com.typesafe.play"             %% "play-akka-http-server"         % playVersion,
      "com.typesafe.akka"             %% "akka-actor-typed"              % akkaVersion
    )
  )
  .dependsOn(akkaHttp, http4s, catsInterop, finch, play, monixInterop, tapirInterop, clientJVM, federation, zioHttp)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(
    crossScalaVersions -= scala3,
    libraryDependencies ++= Seq(
      "org.sangria-graphql" %% "sangria"       % "2.0.0",
      "org.sangria-graphql" %% "sangria-circe" % "1.3.0"
    )
  )

lazy val federation = project
  .in(file("federation"))
  .settings(name := "caliban-federation")
  .settings(commonSettings)
  .dependsOn(core % "compile->compile;test->test")
  .settings(
    crossScalaVersions -= scala3,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
    ),
    Compile / PB.targets := Seq(
      scalapb.gen(grpc = false) -> (Compile / sourceManaged).value / "scalapb"
    ),
    libraryDependencies ++= Seq(
      "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
    ),
    scalacOptions += "-Ywarn-unused:-locals"
  )

val commonSettings = Def.settings(
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
