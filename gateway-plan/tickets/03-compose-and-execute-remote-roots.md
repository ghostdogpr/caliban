# 03 — Compose and execute multiple remote roots

**Outcome:** One gateway composes compatible pinned schemas and executes root fields owned by different remote subgraphs.

**Blocked by:** 02 — Execute one pinned remote graph end to end

**Status:** completed

## Completion criteria

- [x] Unique compatible query and mutation root fields from several subgraphs appear in one client schema.
- [x] Incompatible contributions and duplicate root ownership produce accumulated, deterministic, source-attributed diagnostics and no runtime.
- [x] Query root work without dependencies may run concurrently; response fields remain in client order.
- [x] Data and GraphQL errors from each source are merged at their client paths without losing independent successful data.
- [x] Client introspection runs against the composed schema without a remote call.
- [x] An end-to-end test executes one query spanning two remote root sources.
- [x] Composition and routing expose small interfaces; schema categorization helpers remain local unless reused by another deep module.

## Implementation note

`SchemaComposition` is the single composition seam: `Gateway.build` gives it validated source `RootType`s, and
`RemoteGatewayRuntime` consumes its `ComposedGraph` for client validation, introspection, and root ownership. Routing
uses Caliban's prepared top-level `Field`s directly. A one-source operation without gateway-local meta fields keeps the
original Ticket 2 request path; only genuinely composed work is rendered into source-specific operations. The private
`Field.collectFields` seam is shared by Caliban's executor and gateway routing so repeated selections retain Caliban's
collection semantics. Split operations reject custom executable directives until Ticket 15. Mutations remain visible
in the composed schema, but operations spanning multiple mutation owners fail safely until Ticket 16.
Caliban now exposes its implicit introspection meta-fields alongside application query roots, allowing mixed operations
to validate while the gateway executes their introspection fields locally.
