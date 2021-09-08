import sbt._

object Libraries {

  val zioVersion     = "1.0.11"
  val sttpVersion    = "3.3.14"

  val zio     = "dev.zio" %% "zio"         % zioVersion
  val prelude = "dev.zio" %% "zio-prelude" % "1.0.0-RC6"

  val calibanLibs =
    sys.props.get("plugin.version") match {
      case Some(x) => "com.github.ghostdogpr" %% "caliban" % x,
      case _       => sys.error("""|The system property 'plugin.version' is not defined.
                                   |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
    }

  val sttp = Seq(
    "com.softwaremill.sttp.client3" %% "core"                          % sttpVersion,
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % sttpVersion
  )

}
