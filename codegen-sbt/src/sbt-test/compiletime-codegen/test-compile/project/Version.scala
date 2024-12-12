object Version {
  def pluginVersion: String =
    sys.props.get("plugin.version") match {
      case Some(x) => x
      case _       => sys.error("""|The system property 'plugin.version' is not defined.
                             |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
    }
  def zioTestVersion: String =
    sys.props.get("zio.test.version") match {
      case Some(x) => x
      case _       => sys.error("""|The system property 'zio.test.version' is not defined.
                             |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
    }
  def sttpVersion: String =
    sys.props.get("sttp.version") match {
      case Some(x) => x
      case _       => sys.error("""|The system property 'sttp.version' is not defined.
                             |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
    }
}
