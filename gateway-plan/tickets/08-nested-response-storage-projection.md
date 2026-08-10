# 08 — Store and project nested objects and lists

**What to build:** Extend the indexed response store and prepared-selection writer to handle nested objects and dynamic lists while retaining internal routing data separately from client projection.

**Blocked by:** 07 — Execute and project a flat root query

**Status:** ready-for-agent

- [ ] Planned object and field slots use compact indexed storage rather than a recursive ResponseValue routing tree.
- [ ] Dynamic lists use bounded contiguous handles and preserve client order.
- [ ] Internal key and requirement values remain addressable to execution but never appear in client projection.
- [ ] Nested structured responses are semantically correct for nullable objects, lists, aliases, and repeated selections.
- [ ] Response memory and projection output remain bounded.

