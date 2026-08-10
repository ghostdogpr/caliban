# 39 — Add operation resolution and policy

**What to build:** Install typed operation resolution before parsing and whole-operation policy after planned-operation lookup and variable binding, preserving cache and routing invariants.

**Blocked by:** 29 — Complete ownership and visibility composition; 35 — Add bounded caches and single-flight; 37 — Track overdue work and narrow deadlines

**Status:** ready-for-agent

- [ ] Resolver returns canonical query text or a closed typed failure and cannot rewrite variables, operation name, schema, validation, or plans.
- [ ] Policy sees only the stable read-only operation/security view and required coerced inputs.
- [ ] Policy may allow or reject the whole operation but cannot rewrite selections, variables, visibility, plan, or source choice.
- [ ] Composed authenticated, scope, or policy metadata fails build when no policy is installed.
- [ ] Resolver and policy interruption/deadline behavior follows the single request ownership protocol.
- [ ] Cross-matrix compile-and-run examples prove R intersections across multiple local sources, effectful headers, resolver, policy, Subgraph, Gateway, and GatewayRuntime.

