import sbt.librarymanagement.Resolver

resolvers += Resolver.mavenLocal
resolvers += Resolver.publishMavenLocal

addSbtPlugin("com.github.sbt"            % "sbt-native-packager" % "1.9.4")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix"        % "0.9.30")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt"        % "2.4.3")
addSbtPlugin("pl.project13.scala"        % "sbt-jmh"             % "0.4.3")
addSbtPlugin("com.timushev.sbt"          % "sbt-updates"         % "0.6.0")
addSbtPlugin("com.eed3si9n"              % "sbt-buildinfo"       % "0.10.0")
addSbtPlugin("ch.epfl.scala"             % "sbt-bloop"           % "1.4.8")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"       % "1.8.2")
addSbtPlugin("au.com.onegeek"            % "sbt-dotenv"          % "2.1.233")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"        % "0.1.20")

sys.props.get("plugin.version") match {
  case Some(x) => addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % x)
  case _       => sys.error("""|The system property 'plugin.version' is not defined.
                         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin)
}
