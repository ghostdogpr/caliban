# 44 — Harden Quick ingress and install header context

**What to build:** Install the core incoming-header context around Quick execution and bring unary HTTP ingress and method handling to the reviewed finite GraphQL-over-HTTP behavior.

**Blocked by:** 02 — Establish the shared Caliban core seams; 23 — Apply runtime header policy

**Status:** ready-for-agent

- [ ] Quick installs IncomingHeaders around structured and encoded calls without widening the interpreter environment.
- [ ] An explicit embedded runtime header argument takes precedence over the FiberRef fallback.
- [ ] Raw unary request bodies are bounded before materializing strings, arrays, or GraphQLRequest values.
- [ ] Body overflow returns 413 and malformed or empty request encoding preserves the intended 400 behavior.
- [ ] Mutation-over-GET returns 405 with Allow: POST on both response media types.
- [ ] The finite body limit has a documented default and cannot be disabled.

