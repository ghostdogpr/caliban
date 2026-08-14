# 14 — Add structural schema transformations

**Outcome:** Code-first transformations can rename or hide supported schema coordinates while keeping execution routing correct.

**Blocked by:** 08 — Add explicit ordinary GraphQL lookups; 13 — Complete ownership and visibility composition

**Status:** ready-for-agent

## Completion criteria

- [ ] Supported type, field, argument, input-field, and enum transformations alter the client schema and reverse-map client operations to source coordinates.
- [ ] Keys, lookups, requirements, provided fields, and error paths use transformed coordinates consistently.
- [ ] Invalid or colliding transformations fail composition with source-attributed diagnostics.
- [ ] Transformations are immutable structural descriptions, not arbitrary execution hooks.
- [ ] Remote, Federation, and local source tests exercise the same transformation implementation.
- [ ] The implementation extends one coordinate-mapping module as new coordinate families become real.
