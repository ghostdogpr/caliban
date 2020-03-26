# Examples

To run the examples without the rest of the project, add this to your build.sbt:

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban" % "0.7.3",
  "com.github.ghostdogpr" %% "caliban-http4s" % "0.7.3",
  "com.github.ghostdogpr" %% "caliban-akka-http" % "0.7.3",
  "de.heikoseeberger"     %% "akka-http-circe" % "1.31.0",
  "com.github.ghostdogpr" %% "caliban-cats" % "0.7.3",
  "com.github.ghostdogpr" %% "caliban-finch" % "0.7.3"
)

scalacOptions += "-Ypartial-unification"
```

Run `ExampleApp` and open [http://localhost:8088/graphiql](http://localhost:8088/graphiql)
