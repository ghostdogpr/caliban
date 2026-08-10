# 31 — Support operation conditions and selection details

**What to build:** Complete operation normalization, planning, execution, and projection for aliases, fragments, directives, variable conditions, and null entity keys.

**Blocked by:** 30 — Plan and execute abstract selections

**Status:** ready-for-agent

- [ ] Aliases and fragments preserve GraphQL merge and client-path semantics.
- [ ] Literal conditions fold during normalization.
- [ ] Variable conditions compile into bounded Boolean programs and suppress eligible source calls without replanning.
- [ ] Null or unusable keys skip or fail transitions according to GraphQL and gateway semantics.
- [ ] Projection preserves deterministic client selection order under all supported conditions.
- [ ] Reference-oracle scenarios cover aliases, fragments, conditions, null keys, and varied source completion order.
