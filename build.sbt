import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val mainScala = "2.12.10"
val allScala  = Seq("2.13.1", mainScala)

val http4sVersion   = "0.21.1"
val silencerVersion = "1.6.0"
val sttpVersion     = "2.0.2"
val zioVersion      = "1.0.0-RC17"

inThisBuild(
  List(
    organization := "com.github.ghostdogpr",
    homepage := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    parallelExecution in Test := false,
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
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
    )
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value

name := "caliban"
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(skip in publish := true)
  .settings(historyPath := None)
  .aggregate(
    coreJVM,
    coreJS,
    finch,
    http4s,
    akkaHttp,
    catsInteropJVM,
    catsInteropJS,
    monixInterop,
    clientJVM,
    clientJS,
    codegen
  )

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %%% "fastparse"        % "2.2.4",
      "com.propensive" %%% "magnolia"         % "0.12.7",
      "com.propensive" %%% "mercator"         % "0.2.1",
      "dev.zio"        %%% "zio"              % zioVersion,
      "dev.zio"        %%% "zio-streams"      % zioVersion,
      "dev.zio"        %%% "zio-test"         % zioVersion % "test",
      "dev.zio"        %%% "zio-test-sbt"     % zioVersion % "test",
      "io.circe"       %%% "circe-derivation" % "0.12.0-M7" % Optional,
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
    )
  )
  .jvmSettings(
    fork in Test := true,
    fork in run := true
  )
lazy val coreJVM = core.jvm
lazy val coreJS = core.js.settings(
  libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-RC3" % Test
)
lazy val codegen = project
  .in(file("codegen"))
  .settings(name := "caliban-codegen")
  .settings(commonSettings)
  .settings(
    sbtPlugin := true,
    crossScalaVersions := Seq("2.12.10"),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "scalafmt-dynamic" % "2.4.2",
      "org.scalameta" %% "scalafmt-core"    % "2.4.2",
      "dev.zio"       %% "zio-test"         % zioVersion % "test",
      "dev.zio"       %% "zio-test-sbt"     % zioVersion % "test"
    )
  )
  .dependsOn(coreJVM)

lazy val catsInterop = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("interop/cats"))
  .settings(name := "caliban-cats")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %%% "zio-interop-cats" % "2.0.0.0-RC10",
      "org.typelevel" %%% "cats-effect"      % "2.1.2"
    )
  )
  .dependsOn(core)
lazy val catsInteropJVM = catsInterop.jvm
lazy val catsInteropJS  = catsInterop.js

lazy val monixInterop = project
  .in(file("interop/monix"))
  .settings(name := "caliban-monix")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"  %% "zio-interop-reactivestreams" % "1.0.3.5-RC3",
      "dev.zio"  %% "zio-interop-cats"            % "2.0.0.0-RC10",
      "io.monix" %% "monix"                       % "3.1.0"
    )
  )
  .dependsOn(coreJVM)

lazy val http4s = project
  .in(file("http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-interop-cats"    % "2.0.0.0-RC10",
      "org.typelevel" %% "cats-effect"         % "2.1.2",
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "io.circe"      %% "circe-parser"        % "0.13.0",
      compilerPlugin(
        ("org.typelevel" %% "kind-projector" % "0.11.0")
          .cross(CrossVersion.full)
      ),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full
    )
  )
  .dependsOn(coreJVM)

lazy val akkaHttp = project
  .in(file("akka-http"))
  .settings(name := "caliban-akka-http")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"       % "10.1.11",
      "com.typesafe.akka" %% "akka-stream"     % "2.6.3",
      "de.heikoseeberger" %% "akka-http-circe" % "1.31.0",
      "io.circe"          %% "circe-parser"    % "0.13.0",
      compilerPlugin(
        ("org.typelevel" %% "kind-projector" % "0.11.0")
          .cross(CrossVersion.full)
      )
    )
  )
  .dependsOn(coreJVM)

lazy val finch = project
  .in(file("finch"))
  .settings(name := "caliban-finch")
  .settings(commonSettings)
  .settings(
    crossScalaVersions := Seq("2.12.10"),
    libraryDependencies ++= Seq(
      "com.github.finagle" %% "finchx-core"      % "0.31.0",
      "com.github.finagle" %% "finchx-circe"     % "0.31.0",
      "dev.zio"            %% "zio-interop-cats" % "2.0.0.0-RC10",
      "org.typelevel"      %% "cats-effect"      % "2.1.2"
    )
  )
  .dependsOn(coreJVM)

lazy val client = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("client"))
  .settings(name := "caliban-client")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "io.circe"                     %%% "circe-derivation" % "0.12.0-M7",
      "com.softwaremill.sttp.client" %%% "core"             % sttpVersion,
      "com.softwaremill.sttp.client" %%% "circe"            % sttpVersion,
      "dev.zio"                      %%% "zio-test"         % zioVersion % "test",
      "dev.zio"                      %%% "zio-test-sbt"     % zioVersion % "test"
    )
  )
lazy val clientJVM = client.jvm
lazy val clientJS  = client.js

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(skip in publish := true)
  .settings(
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client" %% "async-http-client-backend-zio" % sttpVersion
    )
  )
  .dependsOn(akkaHttp, http4s, catsInteropJVM, finch, monixInterop, clientJVM)

lazy val benchmarks = project
  .in(file("benchmarks"))
  .settings(commonSettings)
  .settings(skip in publish := true)
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.sangria-graphql" %% "sangria"       % "1.4.2",
      "org.sangria-graphql" %% "sangria-circe" % "1.2.1"
    )
  )

val commonSettings = Def.settings(
  scalaVersion := mainScala,
  crossScalaVersions := allScala,
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
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-opt-inline-from:<source>",
        "-opt-warnings",
        "-opt:l:inline"
      )
    case _ => Nil
  })
)
