# 10 — Execute client introspection locally

**What to build:** Serve client introspection from the composed client schema as local planned work, including operations that mix introspection fields with routed data.

**Blocked by:** 04 — Prepare and check client operations; 08 — Store and project nested objects and lists

**Status:** ready-for-agent

- [ ] __schema and __type execute locally and never call an execution source.
- [ ] Legal root __typename fields execute locally.
- [ ] An operation may combine local introspection and routed data while preserving client field order.
- [ ] Normal validation governs legal meta-field placement.
- [ ] Internal routing metadata is not exposed by introspection.

