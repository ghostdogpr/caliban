# 15 — Classify and own every remote response

**What to build:** Complete the remote protocol and body-ownership matrix on top of the MVP SourceResult versus SourceFailure model, including legacy media behavior and bounded source-document lifetimes.

**Blocked by:** 07 — Perform one bounded and classified remote GraphQL call; 14 — Close the structured Federation MVP

**Status:** ready-for-agent

- [ ] A well-formed application/json GraphQL envelope is accepted only for a 2xx response.
- [ ] Legacy JSON non-2xx, missing or unsupported media types, empty or 204 responses, malformed or truncated JSON, and invalid envelopes become typed SourceFailure.
- [ ] Decoded-body, token, and nesting limits are enforced after content decoding.
- [ ] SourceDocument owns bounded UTF-8 buffers and any raw references remain valid only while their explicit leases are held.
- [ ] Every failure path releases response resources and exposes no endpoint, body, throwable, or unsafe upstream text.

