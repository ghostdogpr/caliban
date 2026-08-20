# Caliban Gateway Language

Use these terms in gateway code, tickets, and discussion. Prefer ordinary GraphQL and Caliban names over gateway-specific synonyms.

**Graph** — the client-visible GraphQL schema and behavior exposed by one gateway runtime.

**Subgraph** — one named schema contribution together with the means to execute fields it owns. A subgraph may be remote GraphQL or an in-process Caliban graph.

**Composition** — combining subgraph schemas and routing metadata into a graph. Composition reports source-attributed diagnostics and either succeeds completely or produces no runtime.

**Composed graph** — the immutable result of composition: client schema plus the metadata required to route operations. It is an in-memory implementation detail, not a serialized artifact.

**Route** — the source and source operation chosen to satisfy a client selection. A route may depend on values produced by an earlier route.

**Plan** — the immutable description of the routes needed for one client operation. The implementation should have one authoritative plan representation until measurement proves that another representation earns its cost.

**Execution source** — something that can execute a routed GraphQL selection. Remote GraphQL and local Caliban are the initial implementations of this real seam.

**Entity transition** — moving from a value produced by one source to an explicitly declared lookup in another source. Federation `_entities` is one kind of lookup.

**Key** — fields that identify an entity for a particular lookup. Matching type or field names alone never create a cross-source route.

**Requirement** — fields needed to perform a later route even when the client did not request them. Requirements stay internal to execution and are absent from the projected result.

**Security requirement** — authorization metadata composed for a selected schema coordinate, including any runtime type condition under which it applies. An operation policy decides it; the gateway does not assign application-specific authorization meaning.

**Composed directive metadata** — compatible directive definitions and applications intentionally retained from contributing subgraphs on composed schema coordinates. Retaining metadata does not make the gateway interpret that directive during execution.

**Source result** — a valid GraphQL response from an execution source, including partial data and GraphQL errors.

**Source failure** — failure to obtain a valid GraphQL response, such as a transport error, invalid protocol response, timeout, or size violation. It is distinct from GraphQL errors in a valid source result.

**Gateway runtime** — the scoped, concurrently reusable result of `Gateway.build`. It is compatible with Caliban's interpreter interface and owns gateway-created resources.

**Graph generation** — the immutable schema-dependent state owned by one gateway runtime. Cached values and plans never cross graph generations.

**Structured response** — a normal Caliban `GraphQLResponse` containing `ResponseValue` data.

**Encoded response** — an optional wire-ready response produced without unnecessary intermediate serialization. It must preserve the structured response's semantics.

**Useful throughput** — responses per unit of time that also pass the workload's semantic assertions. Incorrect or unexpectedly rejected responses do not count.
