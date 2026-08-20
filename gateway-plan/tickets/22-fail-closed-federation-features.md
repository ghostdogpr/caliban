# 22 — Fail closed on security and unsupported Federation features

**Outcome:** Federation directives that affect authorization or routing cannot compose successfully while their semantics are ignored.

**Blocked by:** 14 — Add structural schema transformations; 15 — Support abstract and conditional operations; 19 — Add operation resolution and policy hooks; 20 — Add bounded caches and admission

**Status:** ready-for-agent

## Completion criteria

- [ ] Federation-related directives are identified through their linked specification identity, including imports, aliases, and namespace-qualified names; unrelated application directives with the same local names retain ordinary GraphQL behavior.
- [ ] `@authenticated`, `@requiresScopes`, and `@policy` applications are retained losslessly on their composed schema coordinates, including scope/policy grouping and type-level requirements that apply to selected fields.
- [ ] `OperationPolicy.ValidatedOperation` exposes the security requirements for validated selections in a compact immutable form: aliases and fragments are resolved, `@skip`/`@include` decisions use coerced variables, and runtime type conditions remain explicit when they cannot be known before execution.
- [ ] A graph containing security requirements fails to build with source-attributed diagnostics unless an operation policy is installed. With a policy, every request, including a plan-cache hit, is decided before any local or remote source work begins.
- [ ] Progressive `@override(label:)`, `@context`, and `@fromContext` produce deterministic unsupported-feature diagnostics during composition. Basic unlabeled `@override(from:)` keeps its existing behavior.
- [ ] Tests cover imported aliases, namespace forms, type- and field-level security, cached operations, policy rejection, and zero source calls for every fail-closed path.
- [ ] The change deepens the existing composition and operation-policy seams; it does not add built-in authorization rules or a general middleware system.
