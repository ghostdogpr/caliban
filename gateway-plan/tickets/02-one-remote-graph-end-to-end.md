# 02 — Execute one pinned remote graph end to end

**Outcome:** A gateway built from one ordinary remote subgraph with pinned SDL executes a nested query through `GatewayRuntime` and returns normal Caliban data and errors.

**Blocked by:** 01 — Keep the gateway foundation

**Status:** ready-for-agent

## Completion criteria

- [ ] Pinned SDL and parsed schema documents use Caliban parsing and schema validation through one build path.
- [ ] A small, reviewed Caliban seam supplies the gateway with an already validated client operation; the gateway does not implement a parallel GraphQL operation frontend.
- [ ] Variables, aliases, fragments, directives, nested objects, lists, and introspection retain ordinary Caliban behavior for a single subgraph.
- [ ] The remote source sends one GraphQL-over-HTTP POST and accepts valid data, errors, or partial data plus errors.
- [ ] Transport or invalid-response failure becomes a safe gateway-authored execution error.
- [ ] One stub-server test exercises build and nested execution only through `GatewayRuntime`.
- [ ] The implementation has one operation/route representation and uses normal `GraphQLResponse`/`ResponseValue` results.
