# 47 — Profile the engine and set measured defaults

**What to build:** Measure component, in-process, and external gateway behavior to identify dominant performance seams and replace architectural guesses with evidence-based finite defaults.

**Blocked by:** 46 — Build the production benchmark gate

**Status:** ready-for-agent

- [ ] Profiles report latency, CPU, allocation, GC, memory/live set, and useful throughput.
- [ ] Component measurements isolate frontend, planning, source ingestion, integration, and projection costs.
- [ ] In-process measurements separate engine overhead from remote transport effects.
- [ ] External measurements use the production encoded Quick path and reproducible gate profile.
- [ ] Finite cache, queue, concurrency, body, response, and planner defaults are justified by measured behavior.
- [ ] Potential transport, raw scanning, pooling, or local-preparation specialization is ranked by evidence rather than implemented speculatively.

