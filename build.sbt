import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val mainScala       = "2.12.10"
val allScala        = Seq("2.13.1", mainScala)
val http4sVersion   = "0.21.0-RC4"
val silencerVersion = "1.4.4"
inThisBuild(
  List(
    organization := "com.github.ghostdogpr",
    homepage := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    scalaVersion := mainScala,
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
    ),
    crossScalaVersions := allScala
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
  .aggregate(coreJVM, coreJS, http4s, akkaHttp, catsInteropJVM, catsInteropJS, codegen)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %%% "fastparse"        % "2.2.3",
      "com.propensive" %%% "magnolia"         % "0.12.6",
      "com.propensive" %%% "mercator"         % "0.2.1",
      "dev.zio"        %%% "zio"              % "1.0.0-RC17",
      "dev.zio"        %%% "zio-streams"      % "1.0.0-RC17",
      "dev.zio"        %%% "zio-test"         % "1.0.0-RC17" % "test",
      "dev.zio"        %%% "zio-test-sbt"     % "1.0.0-RC17" % "test",
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
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "scalafmt-dynamic" % "2.3.3-RC2",
      "com.geirsson"  %%% "scalafmt-core"    % "1.5.1",
      "dev.zio"       %%% "zio-test"         % "1.0.0-RC17" % "test",
      "dev.zio"       %%% "zio-test-sbt"     % "1.0.0-RC17" % "test"
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
      "org.typelevel" %%% "cats-effect"      % "2.1.0"
    )
  )
  .dependsOn(core)
lazy val catsInteropJVM = catsInterop.jvm
lazy val catsInteropJS  = catsInterop.js

lazy val http4s = project
  .in(file("http4s"))
  .settings(name := "caliban-http4s")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-interop-cats"    % "2.0.0.0-RC10",
      "org.typelevel" %% "cats-effect"         % "2.1.0",
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "io.circe"      %% "circe-parser"        % "0.12.3",
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
      "de.heikoseeberger" %% "akka-http-circe" % "1.30.0",
      "io.circe"          %% "circe-parser"    % "0.12.3",
      compilerPlugin(
        ("org.typelevel" %% "kind-projector" % "0.11.0")
          .cross(CrossVersion.full)
      )
    )
  )
  .dependsOn(coreJVM)

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(skip in publish := true)
  .dependsOn(akkaHttp, http4s, catsInteropJVM)

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
