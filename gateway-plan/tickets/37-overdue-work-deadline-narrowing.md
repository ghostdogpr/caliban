# 37 — Track overdue work and narrow deadlines

**What to build:** Extend the single MVP deadline protocol with overdue accounting, per-source narrowing, and observability for uninterruptible local, header, resolver, and policy effects without reimplementing request timeout ownership.

**Blocked by:** 21 — Execute local Caliban graphs; 24 — Apply runtime header policy; 25 — Configure source execution behavior; 36 — Implement admission, drain, and runtime status

**Status:** ready-for-agent

- [ ] Per-source timeouts narrow the absolute request deadline and cover permit wait, attempts, backoff, ingestion, and SourceResult transfer.
- [ ] An admitted request becomes overdue when its deadline fires and its structured request tree has not exited.
- [ ] GatewayStatus exposes overdue counts separately from admitted and queued counts.
- [ ] Uninterruptible user effects retain their environment, inputs, resources, permits, and accounting until they really exit.
- [ ] Late results remain disabled and may complete cleanup only.
- [ ] The implementation reuses the MVP disable-delivery-then-interrupt protocol rather than introducing a second timeout state machine.

