# Caliban

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Build Status][Badge-Circle]][Link-Circle]

[Link-Circle]: https://circleci.com/gh/ghostdogpr/caliban "circleci"
[Badge-Circle]: https://circleci.com/gh/ghostdogpr/caliban.svg?style=svg "circleci"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/com/github/ghostdogpr/caliban_2.12/ "Sonatype Releases"
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.ghostdogpr/caliban_2.12.svg "Sonatype Releases"

Caliban is a purely functional library for creating GraphQL backends in Scala.
It relies on [Magnolia](https://github.com/propensive/magnolia) to automatically derives GraphQL schemas from your data types, [Fastparse](https://github.com/lihaoyi/fastparse) to parse queries and [ZIO](https://github.com/zio/zio) to handle various effects.

The design principles behind the library are the following:
- pure interface: errors and effects are returned explicitly (no exceptions thrown), all returned types are referentially transparent (no `Future`).
- clean separation between schema definition and implementation: schema is defined and validated at compile time using Scala standard types, resolver (`RootResolver`) is a simple value provided at runtime.
- minimal amount of boilerplate: no need to manually define a schema for every type in your API.

### Consult the [Documentation](https://ghostdogpr.github.io/caliban/docs/) to learn how to use Caliban.

### Any questions? Head up to the [#caliban](https://discordapp.com/channels/629491597070827530/633200096393166868) channel on [ZIO Discord](https://discord.gg/EYpumuv).
