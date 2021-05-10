# Caliban

[![Build Status][Badge-Circle]][Link-Circle]
[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]
[![Badge-Scaladoc]][Link-Scaladoc]
[![Badge-Discord]][Link-Discord]

[Link-Circle]: https://circleci.com/gh/ghostdogpr/caliban "circleci"
[Badge-Circle]: https://circleci.com/gh/ghostdogpr/caliban.svg?style=shield "circleci"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/com/github/ghostdogpr/caliban_2.12/ "Sonatype Releases"
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.ghostdogpr/caliban_2.12.svg "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/com/github/ghostdogpr/caliban_2.12/ "Sonatype Snapshots"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/com.github.ghostdogpr/caliban_2.12.svg "Sonatype Snapshots"
[Link-Scaladoc]: https://javadoc.io/doc/com.github.ghostdogpr/caliban_2.12/latest/caliban/index.html
[Badge-Scaladoc]: https://javadoc-badge.appspot.com/com.github.ghostdogpr/caliban_2.12.svg?label=scaladoc "Scaladoc"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"

Caliban is a purely functional library for building GraphQL servers and clients in Scala.
 
The design principles behind the library are the following:
- minimal amount of boilerplate: no need to manually define a schema for every type in your API.
- pure interface: errors and effects are returned explicitly (no exceptions thrown), all returned types are referentially transparent (no `Future`).
- clean separation between schema definition and implementation: schema is defined and validated at compile time using Scala standard types, resolver (`RootResolver`) is a simple value provided at runtime.

### Consult the [Documentation](https://ghostdogpr.github.io/caliban/docs/) to learn how to use Caliban.

### Any questions? Head up to the [#caliban](https://discordapp.com/channels/629491597070827530/633200096393166868) channel on [ZIO Discord](https://discord.gg/EYpumuv).

### Adopters

Here is a partial list of companies using Caliban in production.

Want to see your company here? [Submit a PR](https://github.com/ghostdogpr/caliban/edit/master/README.md)!

* [AutoScout24](https://www.autoscout24.de)
* [Carvana](https://www.carvana.com)
* [Credimi](https://www.credimi.com)
* [LeadIQ](https://leadiq.com)
* [Sanjagh.pro](https://sanjagh.pro)
