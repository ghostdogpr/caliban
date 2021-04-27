import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val mainScala = "2.12.13"
val allScala  = Seq("2.13.5", mainScala)

val akkaVersion           = "2.6.14"
val catsEffectVersion     = "2.5.0"
val circeVersion          = "0.13.0"
val http4sVersion         = "0.21.22"
val magnoliaVersion       = "0.17.0"
val mercatorVersion       = "0.2.1"
val playVersion           = "2.8.8"
val playJsonVersion       = "2.9.2"
val silencerVersion       = "1.7.3"
val sttpVersion           = "3.3.0"
val tapirVersion          = "0.17.18"
val zioVersion            = "1.0.7"
val zioInteropCatsVersion = "2.4.1.0"
val zioConfigVersion      = "1.0.4"
val zqueryVersion         = "0.2.7"
val zioJsonVersion        = "0.1.4"

inThisBuild(
  List(
    scalaVersion := mainScala,
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
    libraryDependencies ++= Seq(
      "com.propensive" %% "magnolia" % magnoliaVersion,
      "com.propensive" %% "mercator" % mercatorVersion
    )
  )

lazy val core = project
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi"       %% "fastparse"    % "2.3.2",
      "com.propensive"    %% "magnolia"     % magnoliaVersion,
      "com.propensive"    %% "mercator"     % mercatorVersion,
      "dev.zio"           %% "zio"          % zioVersion,
      "dev.zio"           %% "zio-streams"  % zioVersion,
      "dev.zio"           %% "zio-query"    % zqueryVersion,
      "dev.zio"           %% "zio-test"     % zioVersion      % "test",
      "dev.zio"           %% "zio-test-sbt" % zioVersion      % "test",
      "io.circe"          %% "circe-core"   % circeVersion    % Optional,
      "com.typesafe.play" %% "play-json"    % playJsonVersion % Optional,
      "dev.zio"           %% "zio-json"     % zioJsonVersion  % Optional,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
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
    crossScalaVersions := Seq("2.12.13"),
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
    libraryDependencies ++= Seq(
      "dev.zio"  %% "zio-interop-reactivestreams" % "1.0.3.5-RC12",
      "dev.zio"  %% "zio-interop-cats"            % zioInteropCatsVersion,
      "io.monix" %% "monix"                       % "3.3.0"
    )
  )
  .dependsOn(core)

lazy val tapirInterop = project
  .in(file("interop/tapir"))
  .settings(name := "caliban-tapir")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir"   %% "tapir-core"     % tapirVersion,
      "dev.zio"                       %% "zio-test"       % zioVersion % "test",
      "dev.zio"                       %% "zio-test-sbt"   % zioVersion % "test",
      compilerPlugin(("org.typelevel" %% "kind-projector" % "0.11.3").cross(CrossVersion.full))
    )
  )
  .dependsOn(core)

lazy val http4s = project
  .in(file("adapters/http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"          %% "zio-interop-cats"    % zioInteropCatsVersion,
      "org.typelevel"    %% "cats-effect"         % catsEffectVersion,
      "org.http4s"       %% "http4s-dsl"          % http4sVersion,
      "org.http4s"       %% "http4s-circe"        % http4sVersion,
      "org.http4s"       %% "http4s-blaze-server" % http4sVersion,
      "io.circe"         %% "circe-parser"        % circeVersion,
      compilerPlugin(
        ("org.typelevel" %% "kind-projector"      % "0.11.3")
          .cross(CrossVersion.full)
      ),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
      "com.github.ghik"   % "silencer-lib"        % silencerVersion % Provided cross CrossVersion.full
    )
  )
  .dependsOn(core)

lazy val akkaHttp = project
  .in(file("adapters/akka-http"))
  .settings(name := "caliban-akka-http")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"                  % "10.2.4",
      "com.typesafe.akka" %% "akka-serialization-jackson" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream"                % akkaVersion,
      "de.heikoseeberger" %% "akka-http-circe"            % "1.36.0" % Optional,
      "de.heikoseeberger" %% "akka-http-play-json"        % "1.36.0" % Optional,
      compilerPlugin(
        ("org.typelevel"  %% "kind-projector"             % "0.11.3")
          .cross(CrossVersion.full)
      )
    )
  )
  .dependsOn(core)

lazy val finch = project
  .in(file("adapters/finch"))
  .settings(name := "caliban-finch")
  .settings(commonSettings)
  .settings(
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
      "io.circe"                      %%% "circe-core"   % circeVersion,
      "com.softwaremill.sttp.client3" %%% "core"         % sttpVersion,
      "com.softwaremill.sttp.client3" %%% "circe"        % sttpVersion,
      "dev.zio"                       %%% "zio-test"     % zioVersion % "test",
      "dev.zio"                       %%% "zio-test-sbt" % zioVersion % "test"
    )
  )
lazy val clientJVM = client.jvm
lazy val clientJS  = client.js.settings(
  libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.2.2" % Test
)

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .settings(
    libraryDependencies ++= Seq(
      "de.heikoseeberger"             %% "akka-http-circe"               % "1.36.0",
      "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion,
      "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"              % tapirVersion,
      "io.circe"                      %% "circe-generic"                 % circeVersion,
      "com.typesafe.play"             %% "play-akka-http-server"         % playVersion,
      "com.typesafe.akka"             %% "akka-actor-typed"              % akkaVersion
    )
  )
  .dependsOn(akkaHttp, http4s, catsInterop, finch, play, monixInterop, tapirInterop, clientJVM, federation)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(publish / skip := true)
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
  .settings(
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
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-language:higherKinds",
    "-language:existentials",
    "-unchecked",
    "-Xlint:_,-type-parameter-shadow",
    "-Xfatal-warnings",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:patvars,-implicits",
    "-Ywarn-value-discard"
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
        "-opt:l:inline"
      )
    case Some((2, 13)) =>
      Seq(
        "-Xlint:-byname-implicit"
      )
    case _             => Nil
  })
)
