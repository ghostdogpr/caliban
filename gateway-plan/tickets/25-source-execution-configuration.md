# 25 — Configure source execution behavior

**What to build:** Provide immutable typed remote and local execution configuration for permits, request-level deadlines, body and structure limits, error policy, and execution redirects.

**Blocked by:** 16 — Classify and own every remote response; 21 — Execute local Caliban graphs; 24 — Apply runtime header policy

**Status:** ready-for-agent

- [ ] Remote and local sources have distinct logical-call permit limits and configuration appropriate to their execution kind.
- [ ] Per-source deadlines and safety limits may only narrow applicable global/request bounds.
- [ ] Execution redirects are disabled by default; explicitly enabled hops are finite and authorization or cookie headers never cross origins.
- [ ] Backend-level pool, proxy, TLS, protocol, and decompression settings remain global to the single owned transport.
- [ ] Inherited, disabled, and configured facilities use an explicit ADT rather than overloaded absence.
- [ ] Invalid contradictions fail build and valid ineffective values produce deterministic warnings.
