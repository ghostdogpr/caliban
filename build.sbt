val mainScala = "2.12.10"
val allScala  = Seq("2.11.12", mainScala)

inThisBuild(
  List(
    organization := "com.github.ghostdogpr",
    homepage := Some(url("https://github.com/ghostdogpr/caliban")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scalaVersion := mainScala,
    parallelExecution in Test := false,
    fork in Test := true,
    fork in run := true,
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    releaseEarlyWith := SonatypePublisher,
    scmInfo := Some(
      ScmInfo(url("https://github.com/ghostdogpr/caliban/"), "scm:git:git@github.com:ghostdogpr/caliban.git")
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

name := "caliban"
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val root = project
  .in(file("."))
  .settings(skip in publish := true)
  .aggregate(core)

lazy val core = project
  .in(file("core"))
  .settings(name := "caliban")
  .settings(commonSettings)
  .settings(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    libraryDependencies ++= Seq(
      "com.lihaoyi"    %% "fastparse"    % "2.1.3",
      "com.propensive" %% "magnolia"     % "0.11.0",
      "dev.zio"        %% "zio"          % "1.0.0-RC13",
      "dev.zio"        %% "zio-streams"  % "1.0.0-RC13",
      "dev.zio"        %% "zio-test"     % "1.0.0-RC13" % "test",
      "dev.zio"        %% "zio-test-sbt" % "1.0.0-RC13" % "test",
      compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0")
    )
  )

lazy val examples = project
  .in(file("examples"))
  .settings(commonSettings)
  .settings(skip in publish := true)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"       %% "zio-interop-cats"          % "2.0.0.0-RC3",
      "org.typelevel" %% "cats-effect"               % "2.0.0",
      "org.http4s"    %% "http4s-dsl"                % "0.21.0-M4",
      "org.http4s"    %% "http4s-circe"              % "0.21.0-M4",
      "org.http4s"    %% "http4s-blaze-server"       % "0.21.0-M4",
      "io.circe"      %% "circe-magnolia-derivation" % "0.4.0",
      "io.circe"      %% "circe-parser"              % "0.12.1"
    )
  )
  .dependsOn(core)

val commonSettings = Def.settings(
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-Xfuture",
    "-language:higherKinds",
    "-language:existentials",
    "-unchecked",
    "-Xlint:_,-type-parameter-shadow",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused",
    "-Ywarn-value-discard"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 11)) =>
      Seq(
        "-Yno-adapted-args",
        "-Ypartial-unification",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit"
      )
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
