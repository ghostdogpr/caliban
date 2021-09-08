import sbt._

object Libraries {

  val calibanLibs: Seq[ModuleID] =
    sys.props.get("plugin.version") match {
      case Some(x) => Seq("com.github.ghostdogpr" %% "caliban" % x)
      case _       => sys.error("""|The system property 'plugin.version' is not defined.
                                   |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
    }

  val sttp = Seq(
    "com.softwaremill.sttp.client3" %% "core"                          % "3.3.14",
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % "3.3.14"
  )

}
