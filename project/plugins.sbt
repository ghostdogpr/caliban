val sbtcrossProjectVersion = "1.3.2"

addSbtPlugin("org.scalameta"      % "sbt-scalafmt"                  % "2.5.2")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"                % "1.5.12")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.17.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.5.5")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % sbtcrossProjectVersion)
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % sbtcrossProjectVersion)
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                       % "0.4.7")
addSbtPlugin("com.thesamet"       % "sbt-protoc"                    % "1.0.7")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"                 % "0.12.0")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                      % "2.6.1")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"               % "1.1.4")
addSbtPlugin("com.eed3si9n"       % "sbt-assembly"                  % "2.3.0")

addSbtPlugin("org.jetbrains.scala" % "sbt-ide-settings" % "1.1.2")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.17"
addDependencyTreePlugin
