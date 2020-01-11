# Examples

To run the examples without the rest of the project, add this to your build.sbt:

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban" % "0.4.1",
  "com.github.ghostdogpr" %% "caliban-http4s" % "0.4.1",
  "com.github.ghostdogpr" %% "caliban-akka-http" % "0.4.1",
  "com.github.ghostdogpr" %% "caliban-cats" % "0.4.1",
  "com.beachape" %% "enumeratum"       % "1.5.15",
  "com.beachape" %% "enumeratum-circe" % "1.5.22"


)
```

Run `ExampleApp` and open [http://localhost:8088/graphiql](http://localhost:8088/graphiql)
