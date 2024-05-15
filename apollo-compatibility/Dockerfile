FROM sbtscala/scala-sbt:eclipse-temurin-alpine-22_36_1.10.0_3.4.1 AS build

WORKDIR /build
COPY build.sbt .
COPY project ./project
COPY core ./core
COPY federation ./federation
COPY adapters/quick ./adapters/quick
COPY macros ./macros
COPY apollo-compatibility/src ./apollo-compatibility/src
EXPOSE 4001
CMD sbt apollo-compatibility/run