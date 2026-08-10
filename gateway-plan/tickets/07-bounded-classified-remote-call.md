# 07 — Perform one bounded and classified remote GraphQL call

**What to build:** Prepare and execute one unary GraphQL-over-HTTP source call against a stub server with gateway-owned transport resources, early SourceResult versus SourceFailure classification, and finite ingestion.

**Blocked by:** 01 — Create the fresh gateway foundation; 02 — Establish the shared Caliban core seams

**Status:** ready-for-agent

- [ ] The request is sent as bounded UTF-8 POST JSON with the required Content-Type and Accept values.
- [ ] Media-type-first classification accepts a valid application/graphql-response+json envelope regardless of HTTP status.
- [ ] A valid GraphQL envelope, including partial data and errors, wins over retryable HTTP status classification and becomes SourceResult.
- [ ] Transport errors and non-GraphQL responses become typed SourceFailure rather than GraphQL results.
- [ ] Every attempt owns and releases its request, body, and response resources under success, failure, and interruption.
- [ ] A stub-server suite covers the successful envelope, a valid error envelope, and representative classified failure.

