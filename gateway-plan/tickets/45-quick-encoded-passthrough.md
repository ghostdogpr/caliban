# 45 — Pass encoded gateway responses through Quick

**What to build:** Detect and retain the encoded interpreter capability when constructing Quick routes and return its bytes directly with status, media, validation, and effect-scope parity.

**Blocked by:** 43 — Add the gateway encoded response sink; 44 — Harden Quick ingress and install header context

**Status:** ready-for-agent

- [ ] Quick retains the optional capability before ordinary GraphQLInterpreter combinators can erase its subtype.
- [ ] The configured effect scope surrounds the complete encoded call exactly as for structured execution.
- [ ] Both JSON response formats map typed outcomes to the reviewed status matrix without inspecting encoded bytes.
- [ ] Cache-control metadata follows generic structured parity while the gateway supplies none.
- [ ] Introspection enablement, skip-validation, configured validations, and mutation rejection retain their defined semantics.
- [ ] SSE, uploads, subscriptions, and incremental results remain on the structured path.

