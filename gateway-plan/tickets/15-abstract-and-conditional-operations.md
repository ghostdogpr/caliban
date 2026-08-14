# 15 — Execute abstract and conditional selections

**Outcome:** Routed queries preserve GraphQL selection semantics for aliases, fragments, runtime types, and conditional directives.

**Blocked by:** 10 — Integrate the Federation Gateway Audit; 13 — Complete ownership and visibility composition

**Status:** ready-for-agent

## Completion criteria

- [ ] Interface and union selections route using valid possible runtime types and source capabilities.
- [ ] Inline fragments, fragment spreads, aliases, repeated selections, and `__typename` preserve Caliban semantics across sources.
- [ ] `@skip` and `@include` conditions affect source work and projection without corrupting entity requirements.
- [ ] Missing runtime-type information produces a safe GraphQL outcome rather than an unsafe route.
- [ ] Source errors map through aliases and abstract selections to deterministic client paths.
- [ ] Relevant audit groups pass through the same plan and executor used by ordinary object selections.
