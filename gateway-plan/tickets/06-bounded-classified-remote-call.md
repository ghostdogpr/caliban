# 06 — Perform one bounded and classified remote GraphQL call

**What to build:** Prepare and execute one unary GraphQL-over-HTTP source call against a stub server with gateway-owned transport resources, early SourceResult versus SourceFailure classification, and finite ingestion.

**Blocked by:** 01 — Create the fresh gateway foundation

**Status:** ready-for-agent

- [ ] The request is sent as bounded UTF-8 POST JSON with the required Content-Type and Accept values.
- [ ] Media-type-first classification accepts a valid application/graphql-response+json envelope regardless of HTTP status.
- [ ] A valid GraphQL envelope, including partial data and errors, wins over retryable HTTP status classification and becomes SourceResult.
- [ ] Transport errors and non-GraphQL responses become typed SourceFailure rather than GraphQL results.
- [ ] A finite decoded-response byte cap is enforced before an oversized body can be fully materialized; Ticket 16 owns the exhaustive structure and protocol-limit matrix.
- [ ] Every attempt owns and releases its request, body, and response resources under success, failure, and interruption.
- [ ] A stub-server suite covers the successful envelope, a valid error envelope, and representative classified failure.
