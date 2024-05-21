FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.9.8_2.13.12 AS build

WORKDIR /app
COPY build.sbt .
COPY apollo-compatibility/target/apollo-subgraph-compatibility.jar /app/artifact.jar
EXPOSE 4001
CMD java $* -jar artifact.jar