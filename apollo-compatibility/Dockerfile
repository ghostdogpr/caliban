FROM sbtscala/scala-sbt:graalvm-ce-22.3.3-b1-java17_1.9.8_2.13.12 AS build

WORKDIR /build
COPY build.sbt .
COPY project ./project
COPY core ./core
COPY federation ./federation
COPY adapters/quick ./adapters/quick
COPY macros ./macros
COPY apollo-compatibility/src ./apollo-compatibility/src
EXPOSE 4001
CMD sbt "apolloCompatibility/run"