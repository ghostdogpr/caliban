sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % x)
  case _       => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
conflictWarning := ConflictWarning("warn", Level.Warn, false)
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.5.6")
