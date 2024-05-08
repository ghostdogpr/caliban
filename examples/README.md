# Examples

To run the examples without the rest of the project, add this to your build.sbt:

```scala
val calibanVersion = "2.6.0"

libraryDependencies ++= Seq(
  "com.github.ghostdogpr"         %% "caliban"                       % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-quick"                 % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-http4s"                % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-play"                  % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-akka-http"             % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-pekko-http"            % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-zio-http"              % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-cats"                  % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-federation"            % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-tapir"                 % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-client"                % calibanVersion,
  "com.github.ghostdogpr"         %% "caliban-tools"                 % calibanVersion,
  "org.http4s"                    %% "http4s-ember-server"           % "0.23.23",
  "org.http4s"                    %% "http4s-dsl"                    % "0.23.23",
  "com.softwaremill.sttp.client3" %% "zio"                           % "3.9.0",
  "io.circe"                      %% "circe-generic"                 % "0.14.6",
  "org.playframework"             %% "play-pekko-http-server"        % "3.0.0",
  "com.typesafe.akka"             %% "akka-actor-typed"              % "2.6.18",
  "com.softwaremill.sttp.tapir"   %% "tapir-jsoniter-scala"          % "1.9.4"
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


