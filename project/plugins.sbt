addSbtPlugin("org.scalameta"      % "sbt-scalafmt"             % "2.4.6")
addSbtPlugin("com.github.sbt"     % "sbt-ci-release"           % "1.5.10")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.9.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"                  % "0.4.3")
addSbtPlugin("com.thesamet"       % "sbt-protoc"               % "1.0.6")
addSbtPlugin("com.eed3si9n"       % "sbt-buildinfo"            % "0.10.0")
addSbtPlugin("org.scalameta"      % "sbt-mdoc"                 % "2.3.0")

libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.8"
