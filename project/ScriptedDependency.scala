import sbt.*

//noinspection TypeAnnotation
object ScriptedDependency {
  // scala steward should understand these version in context of dependencies described here
  object Version {
    val sttp    = "3.10.1"
    val zioTest = "2.1.9"
  }

  lazy val sttp = Seq(
    "com.softwaremill.sttp.client3" %% "core" % Version.sttp,
    "com.softwaremill.sttp.client3" %% "zio"  % Version.sttp
  )

  lazy val zioTest = Seq(
    "dev.zio" %% "zio-test"     % Version.zioTest % Test,
    "dev.zio" %% "zio-test-sbt" % Version.zioTest % Test
  )
}
