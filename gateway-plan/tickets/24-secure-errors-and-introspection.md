# 24 — Secure remote errors and verify introspection control

**Outcome:** Untrusted subgraph errors have safe disclosure defaults, while gateway introspection follows Caliban's existing request configuration.

**Blocked by:** 17 — Complete the GraphQL HTTP boundary; 18 — Add source execution policy; 20 — Add bounded caches and admission

**Status:** complete

## Completion criteria

- [x] Remote GraphQL errors keep their rewritten client path and GraphQL completion behavior but redact the subgraph message by default, omit source locations, and retain only the `code` extension by default.
- [x] Operators can opt into remote messages and additional extension keys globally, with a per-subgraph override. Protocol failures and router-authored errors remain safe regardless of this setting.
- [x] Local Caliban errors preserve their existing message, path, and extensions; the remote disclosure policy is applied only at the remote-source boundary.
- [x] Tests cover partial data, aliases, list indices, non-null bubbling, malicious messages/extensions, global defaults, per-subgraph overrides, and errors from both remote and local sources.
- [x] `Configurator.setEnableIntrospection(false)` rejects gateway execution of `__schema` and `__type` on cached and uncached paths, and re-enabling it in another FiberRef context cannot reuse an incompatible cached preparation.
- [x] Caliban's `Configurator` remains the only introspection control and no duplicate flag is added to `GatewayConfig`; Ticket 25 verifies that Quick preserves the same context.
- [x] Error disclosure is owned by the existing remote-source/configuration seam rather than a second response-processing pipeline.

`RemoteGraphQLConfig.ErrorDisclosure` is the one disclosure value used by the gateway-wide default and optional
per-source override. `RemoteGraphQLSource` applies the resolved policy once while decoding a valid GraphQL envelope;
later path rewriting, completion, and local-source execution continue through their existing paths.
