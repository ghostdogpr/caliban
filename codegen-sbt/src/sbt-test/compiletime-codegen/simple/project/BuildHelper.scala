import com.typesafe.sbt.packager.Keys.{dockerBaseImage => _, dockerExposedPorts => _, dockerRepository => _, dockerUpdateLatest => _}
import sbt.Keys._
import sbt.{Compile, _}

object BuildHelper {

  val commonSettings = Seq(
    libraryDependencies += compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    javacOptions ++= Seq("-source", "11", "-target", "11"),
    scalacOptions --= {
      if (sys.env.contains("CI")) Nil else Seq("-Xfatal-warnings") // enforced by the pre-push hook too
    },
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    (Test / parallelExecution) := true,
    (Test / fork)              := true
  ) ++ noDoc

  lazy val noDoc = Seq(
    (Compile / doc / sources)                := Seq.empty,
    (Compile / packageDoc / publishArtifact) := false
  )

  /**
   * Copied from Cats
   */
  lazy val noPublishSettings = Seq(
    publish         := {},
    publishLocal    := {},
    publishM2       := {},
    publishArtifact := false
  )

}
