# 03 — Compose and execute multiple remote roots

**Outcome:** One gateway composes compatible pinned schemas and executes root fields owned by different remote subgraphs.

**Blocked by:** 02 — Execute one pinned remote graph end to end

**Status:** ready-for-agent

## Completion criteria

- [ ] Unique compatible query and mutation root fields from several subgraphs appear in one client schema.
- [ ] Incompatible contributions and duplicate root ownership produce accumulated, deterministic, source-attributed diagnostics and no runtime.
- [ ] Query root work without dependencies may run concurrently; response fields remain in client order.
- [ ] Data and GraphQL errors from each source are merged at their client paths without losing independent successful data.
- [ ] Client introspection runs against the composed schema without a remote call.
- [ ] An end-to-end test executes one query spanning two remote root sources.
- [ ] Composition and routing expose small interfaces; schema categorization helpers remain local unless reused by another deep module.
