# 02 — Execute one pinned remote graph end to end

**Outcome:** A gateway built from one ordinary remote subgraph with pinned SDL executes a nested query through `GatewayRuntime` and returns normal Caliban data and errors.

**Blocked by:** 01 — Keep the gateway foundation

**Status:** completed

## Completion criteria

- [x] Pinned SDL and parsed schema documents use Caliban parsing and schema validation through one build path.
- [x] A small, reviewed Caliban seam supplies the gateway with an already validated client operation; the gateway does not implement a parallel GraphQL operation frontend.
- [x] Variables, aliases, fragments, directives, nested objects, lists, and introspection retain ordinary Caliban behavior for a single subgraph.
- [x] The remote source sends one GraphQL-over-HTTP POST and accepts valid data, errors, or partial data plus errors.
- [x] Transport or invalid-response failure becomes a safe gateway-authored execution error.
- [x] One stub-server test exercises build and nested execution only through `GatewayRuntime`.
- [x] The implementation has one operation/route representation and uses normal `GraphQLResponse`/`ResponseValue` results.

## Implementation note

`caliban.execution.RequestPreparation` is the intentionally introduced operation-front-end seam. The ordinary
Caliban interpreter uses it for parsing, variable coercion, and validation, while `GatewayRuntime` uses it to receive
the same validated `ExecutionRequest` before choosing local introspection or the single remote route.

`caliban.tools.RemoteSchema` owns conversion from parsed schema documents to Caliban schema types. The gateway uses
its package-private validated `RootType` seam; pinned SDL and already parsed documents therefore share extension
normalization, root declaration checks, schema validation, and introspection metadata without a gateway-local model.
