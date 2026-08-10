# 51 — Add optional OpenTelemetry integration

**What to build:** Provide supported tracing in a separate dependency-bearing module after the gateway semantics and hot path are stable.

**Blocked by:** 41 — Add bounded metrics and safe logging; 50 — Verify the useful-throughput gate

**Status:** ready-for-agent

- [ ] Tracing adds request and build/composition spans plus planning spans only on cache miss.
- [ ] One logical source-call span covers permit wait, retries, ingestion, and result handoff; attempts are events or sibling downstream spans.
- [ ] No span is emitted per field, entity, or execution-plan node.
- [ ] Validated W3C context is injected after header policy and cannot be overridden by forwarded values.
- [ ] Baggage is disabled by default and no raw query, variables, headers, bodies, responses, or unsafe messages are captured.
- [ ] The tracing module follows every gateway Scala version supported by its tracing dependencies or records a concrete narrowing reason.

