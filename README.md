# Caliban

[![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases]
[![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots]
[![Badge-Discord]][Link-Discord]

[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/com/github/ghostdogpr/caliban_2.12/ "Sonatype Releases"
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/com.github.ghostdogpr/caliban_2.12.svg "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/com/github/ghostdogpr/caliban_2.12/ "Sonatype Snapshots"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/com.github.ghostdogpr/caliban_2.12.svg "Sonatype Snapshots"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"

Caliban is a purely functional library for building GraphQL servers and clients in Scala.
 
The design principles behind the library are the following:
- minimal amount of boilerplate: no need to manually define a schema for every type in your API.
- high performance: while every public interface is pure and immutable, library internals have been optimized for speed.
- clean separation between schema definition and implementation: schema is defined and validated at compile time using Scala standard types, resolver (`RootResolver`) is a simple value provided at runtime.

### Consult the [Documentation](https://ghostdogpr.github.io/caliban/docs/) to learn how to use Caliban.

### Any questions? Head up to the [#caliban](https://discordapp.com/channels/629491597070827530/633200096393166868) channel on [ZIO Discord](https://discord.gg/EYpumuv).

### Adopters

Here is a partial list of companies using Caliban in production.

Want to see your company here? [Submit a PR](https://github.com/ghostdogpr/caliban/edit/series/2.x/README.md)!

* [Anduin Transactions](https://www.anduintransact.com)
* [AnyMind Group](https://anymindgroup.com)
* [AutoScout24](https://www.autoscout24.de)
* [BusinessChat.io](https://businesschat.io)
* [Carvana](https://www.carvana.com)
* [Conduktor](https://www.conduktor.io)
* [Credimi](https://www.credimi.com)
* [Devsisters](https://www.devsisters.com)
* [Fugo.ai](https://www.fugo.ai)
* [LeadIQ](https://leadiq.com)
* [Norwegian Agency for Shared Services in Education and Research](https://sikt.no/en/home)
* [Sanjagh.pro](https://sanjagh.pro)
* [Soundtrack Your Brand](https://www.soundtrackyourbrand.com)
* [StepZen](https://www.stepzen.com)
* [Undo](https://www.undo.app)
* [Valamis Group](https://www.valamis.com)
