# 23 — Apply runtime header policy

**What to build:** Derive safe source-owned outbound headers from incoming, static, and effectful values while preserving a narrow server-independent incoming-header context.

**Blocked by:** 07 — Perform one bounded and classified remote GraphQL call; 16 — Acquire ordinary and Federation schemas remotely

**Status:** ready-for-agent

- [ ] Policies can select named incoming headers, explicitly forward all, add static or effectful values, and remove names in defined order.
- [ ] Incoming-header reads do not widen R; effectful policy operations do.
- [ ] Names compare case-insensitively and configured values override forwarded values before removals.
- [ ] Hop-by-hop, Connection-named, proxy, transport-owned, and trace-propagation headers cannot be supplied by user policy.
- [ ] Effectful failure becomes a typed source request failure before transport dispatch.
- [ ] Direct embedded headers take precedence over the FiberRef fallback.

