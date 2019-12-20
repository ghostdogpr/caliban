import sbtcrossproject.CrossPlugin.autoImport.{ crossProject, CrossType }

val mainScala = "2.12.10"
val allScala  = Seq("2.13.1", mainScala)

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
  .aggregate(coreJVM, coreJS, http4s, catsInteropJVM, catsInteropJS)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %%% "fastparse"        % "2.2.0",
      "com.propensive" %%% "magnolia"         % "0.12.5",
      "com.propensive" %%% "mercator"         % "0.3.0",
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

lazy val catsInterop = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("interop/cats"))
  .settings(name := "caliban-cats")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %%% "zio-interop-cats" % "2.0.0.0-RC10",
      "org.typelevel" %%% "cats-effect"      % "2.0.0"
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
      "org.typelevel" %% "cats-effect"         % "2.0.0",
      "org.http4s"    %% "http4s-dsl"          % "0.21.0-M6",
      "org.http4s"    %% "http4s-circe"        % "0.21.0-M6",
      "org.http4s"    %% "http4s-blaze-server" % "0.21.0-M6",
      "io.circe"      %% "circe-parser"        % "0.12.3",
      compilerPlugin(
        ("org.typelevel" %% "kind-projector" % "0.11.0")
          .cross(CrossVersion.full)
      )
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
      "com.typesafe.akka" %% "akka-stream"     % "2.5.26",
      "de.heikoseeberger" %% "akka-http-circe" % "1.29.1",
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
