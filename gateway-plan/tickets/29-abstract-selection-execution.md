# 29 — Plan and execute abstract selections

**What to build:** Plan abstract GraphQL selections per possible runtime type and integrate type-specific remote or local results through the common response model.

**Blocked by:** 26 — Support compound keys and multi-hop routing; 28 — Complete ownership and visibility composition

**Status:** ready-for-agent

- [ ] Interfaces, unions, and supported interface-object capabilities expose correct possible runtime types.
- [ ] The planner inserts internal __typename only when needed for safe routing or completion.
- [ ] Type-specific source calls and merge mappings are complete before execution.
- [ ] Runtime results are filtered and integrated according to their concrete type.
- [ ] Internal type-routing data never leaks into the client result.

