# 38 — Implement retries, source outcomes, and masking

**What to build:** Add bounded replay-safe retries and refine source error masking, extension allowlists, and safe message policy on top of the SourceFailure integration semantics established by the MVP.

**Blocked by:** 15 — Classify and own every remote response; 24 — Configure source execution behavior; 37 — Track overdue work and narrow deadlines

**Status:** ready-for-agent

- [ ] Retries are disabled by default and only replay GraphQL query calls.
- [ ] Mutations, local calls, valid GraphQL results, invalid protocol data, TLS certificate failures, and engine defects are never retried.
- [ ] Connection failure, attempt timeout, and configured 500/502/503/504 responses use bounded full-jitter backoff within source and request budgets.
- [ ] One logical source permit covers attempts and backoff; inputs and effectful headers are evaluated once per logical call.
- [ ] Safe gateway-authored messages and stable codes never expose endpoints, bodies, source names, stack traces, or throwables.
- [ ] Source GraphQL error messages, rewritten paths, and configured extension allowlists remain distinct from SourceFailure policy.

