# Examples

To run the examples without the rest of the project, add this to your build.sbt:

```scala
val calibanVersion = "1.1.0"

libraryDependencies ++= Seq(
  "com.github.ghostdogpr"         %% "caliban"                       % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-http4s"                % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-play"                  % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-akka-http"             % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-zio-http"              % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-cats"                  % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-monix"                 % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-finch"                 % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-federation"            % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-tapir"                 % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-client"                % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-tools"                 % calibanVersion,
  "de.heikoseeberger"             %% "akka-http-circe"               % "1.36.0",
  "com.softwaremill.sttp.client3" %% "async-http-client-backend-zio" % "3.2.3",
  "com.softwaremill.sttp.tapir"   %% "tapir-json-circe"              % "0.17.18",
  "io.circe"                      %% "circe-generic"                 % "0.13.0",
  "com.typesafe.play"             %% "play-akka-http-server"         % "2.8.8",
  "com.typesafe.akka"             %% "akka-actor-typed"              % "2.6.14",
)
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


