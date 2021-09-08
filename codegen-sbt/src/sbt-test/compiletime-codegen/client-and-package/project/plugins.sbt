import sbt.librarymanagement.Resolver

resolvers += Resolver.mavenLocal

addSbtPlugin("com.github.sbt"            % "sbt-native-packager" % "1.9.4")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"        % "2.4.3")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"        % "0.1.20")

sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % x)
  case _       => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
