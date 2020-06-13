# Examples

To run the examples without the rest of the project, add this to your build.sbt:

```scala
libraryDependencies ++= Seq(
  "com.github.ghostdogpr"        %% "caliban" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-http4s" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-play" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-akka-http" % "0.8.2",
  "de.heikoseeberger"            %% "akka-http-circe" % "1.32.0",
  "com.github.ghostdogpr"        %% "caliban-cats" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-monix" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-finch" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-uzhttp" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-federation" % "0.8.2",
  "com.github.ghostdogpr"        %% "caliban-tapir" % "0.8.2",
  "com.softwaremill.sttp.tapir"  %% "tapir-json-circe" % "0.14.3",
  "io.circe"                     %% "circe-generic" % "0.13.0"
)

scalacOptions += "-Ypartial-unification"
```

Run `ExampleApp` and open [http://localhost:8088/graphiql](http://localhost:8088/graphiql)

## Federation

If you will be running the federation example, you also need to make sure you have NodeJS installed.

To use the gateway you will need to first need to install the gateway found in the `/resources` folder.

Run by calling:

```
npm i && npm start
```

The gateway should be launched after the federated services. To view open [http://localhost:4000/graphql](http://localhost:4000/graphql).


