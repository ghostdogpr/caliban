# 43 — Add the core encoded response capability

**What to build:** Add the gateway-neutral response classification, structured server failure, encoded response, and optional encoded-interpreter capability to Caliban core without implementing gateway projection or HTTP adaptation.

**Blocked by:** 13 — Close the structured Federation MVP

**Status:** ready-for-agent

- [ ] Core response outcomes distinguish request errors, executed results, and server failures without allowing ordinary execution errors to claim server failure.
- [ ] `GraphQLResponseFormat`, `EncodedGraphQLResponse`, and the optional encoded-interpreter capability are gateway-neutral and introduce no Quick or gateway dependency into core.
- [ ] Json and GraphQLResponseJson formats expose their canonical UTF-8 media types and encoded outcomes require no byte inspection.
- [ ] `CalibanError.ResponseError` carries the same request/server classification and is covered across the Scala matrix, including every Caliban-owned exhaustive match.
- [ ] The sealed-hierarchy compatibility impact is documented and existing structured `GraphQLInterpreter` execution remains source-compatible and behaviorally unchanged.
- [ ] The capability defines caller-owned immutable bytes plus bounded optional cache-control metadata without prescribing how an interpreter produces them.
