# 36 — Implement admission, drain, and runtime status

**What to build:** Add atomic bounded request admission and the Running-to-Draining-to-Closed lifecycle around the established MVP request entry point and deadline boundary.

**Blocked by:** 13 — Close the structured Federation MVP

**Status:** ready-for-agent

- [ ] Queue wait and active registration form one atomic protocol under the already established request deadline.
- [ ] Admission queue and active execution counts are finite.
- [ ] Drain stops admission, wakes or rejects queued waiters, and waits interruptibly for admitted requests.
- [ ] Concurrent drain callers observe one shared completion and interrupting a waiter does not resume the runtime.
- [ ] GatewayStatus reports runtime state plus admitted and queued counts.
- [ ] Scope close interrupts runtime-owned work and closes owned transport only after structured users release it.

