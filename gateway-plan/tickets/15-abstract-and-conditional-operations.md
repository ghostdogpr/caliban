# 15 — Execute abstract and conditional selections

**Outcome:** Routed queries preserve GraphQL selection semantics for aliases, fragments, runtime types, and conditional directives.

**Blocked by:** 10 — Integrate the Federation Gateway Audit; 13 — Complete ownership and visibility composition

**Status:** complete

## Completion criteria

- [x] Interface and union selections route using valid possible runtime types and source capabilities.
- [x] Inline fragments, fragment spreads, aliases, repeated selections, and `__typename` preserve Caliban semantics across sources.
- [x] `@skip` and `@include` conditions affect source work and projection without corrupting entity requirements.
- [x] Missing runtime-type information produces a safe GraphQL outcome rather than an unsafe route.
- [x] Source errors map through aliases and abstract selections to deterministic client paths.
- [x] Relevant audit groups pass through the same plan and executor used by ordinary object selections.
- [x] The pinned audit is rerun without regressions or new deferrals.

## Implementation notes

- Runtime-type evidence is carried by the existing operation plan and retained only for gateway projection and completion.
- Source-specific abstract selections are rendered as valid concrete fragments, including interface-object and source-private requirement types.
- The pinned audit passes 195 of 199 cases. The four remaining failures are the cross-source mutations owned by Ticket 16; no cases are deferred.
