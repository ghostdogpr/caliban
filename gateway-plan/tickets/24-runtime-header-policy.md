# 24 — Apply runtime header policy

**What to build:** Introduce the narrow gateway-neutral incoming-header context in Caliban core, then derive safe source-owned outbound headers from incoming, static, and effectful values.

**Blocked by:** 02 — Normalize pinned subgraph schemas; 06 — Perform one bounded and classified remote GraphQL call

**Status:** ready-for-agent

- [ ] Core `IncomingHeaders` is a validated case-insensitive immutable multi-map with no server-library dependency.
- [ ] Core provides only a narrowly scoped FiberRef fallback for inherited interpreter calls; explicit runtime headers take precedence.
- [ ] Policies can select named incoming headers, explicitly forward all, add static or effectful values, and remove names in defined order.
- [ ] Incoming-header reads do not widen R; effectful policy operations do.
- [ ] Names compare case-insensitively and configured values override forwarded values before removals.
- [ ] Hop-by-hop, Connection-named, proxy, transport-owned, and trace-propagation headers cannot be supplied by user policy.
- [ ] Effectful failure becomes a typed source request failure before transport dispatch.
