# 35 — Add bounded caches and single-flight

**What to build:** Cache parsed, prepared, planned, and deterministic negative results within one graph generation using finite independent budgets and scoped keyed single-flight.

**Blocked by:** 30 — Support operation conditions and selection details

**Status:** ready-for-agent

- [ ] Parsed cache keys include canonical query identity and parser discriminator.
- [ ] Prepared and planned keys include generation, document, selected operation, and every stable validation, visibility, and planning discriminator.
- [ ] Variables, headers, trace context, and ordinary request extensions never enter cache keys.
- [ ] Non-default or unstable validation settings bypass prepared/planned caching until a stable discriminator exists.
- [ ] Cache hits avoid single-flight; waiters cancel independently; abandoned shared work is interrupted.
- [ ] Only successful complete values are inserted and oversized valid operations may execute without cache admission.

