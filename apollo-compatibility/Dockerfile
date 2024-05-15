FROM hseeberger/scala-sbt:17.0.2_1.9.9_3.1.1 AS build

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