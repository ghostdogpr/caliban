# Caliban Federation Gateway Audit adapter

This non-published project runs the pinned Federation Gateway Audit against Caliban's native, code-first gateway composition. For each upstream suite, the adapter fetches the upstream `subgraphs` descriptions, passes their authored SDL and endpoints directly to `Gateway.compose`, and serves the resulting `GatewayRuntime`. It never consumes or creates a serialized supergraph artifact.

The upstream repository and commit live in [`upstream.env`](upstream.env). CI runs the unmodified upstream reporter so its `suite_index` case identities and raw assertion results remain intact. Every case is required to pass unless its suite and case index appear in [`expectations.tsv`](expectations.tsv), which assigns ownership to known failures and explicit deferrals. The report classifies cases as:

- `supported`: failures are regressions and fail CI;
- `failing`: failures remain visible and are assigned to the breadth ticket that owns the missing semantics;
- `deferred`: the fixture requires a non-standard compatibility option outside the current native-composition scope.

`verify-results.sh` combines the raw upstream result with the exception table into a case-level Markdown report. It rejects every unexpected failure, every expected case that disappears, and every stale exception whose case now passes; it never treats an assertion failure as flaky. Intentional improvements therefore require deleting their exception rows.

To run the audit locally, set `CALIBAN_ROOT` to this repository and run the commands from a checkout of the pinned
upstream repository:

```sh
CALIBAN_ROOT=/path/to/caliban
"$CALIBAN_ROOT/gateway-audit/install.sh"
npm start -- test \
  --cwd "$CALIBAN_ROOT/gateway-audit" \
  --run-script ./run.sh \
  --graphql http://127.0.0.1:4000/graphql \
  --healthcheck http://127.0.0.1:4000/health \
  --write "$CALIBAN_ROOT/gateway-audit/results.txt"
. "$CALIBAN_ROOT/gateway-audit/upstream.env"
"$CALIBAN_ROOT/gateway-audit/verify-results.sh" \
  "$CALIBAN_ROOT/gateway-audit/results.txt" \
  "$CALIBAN_ROOT/gateway-audit/expectations.tsv" \
  "$CALIBAN_ROOT/gateway-audit/report.md"
```
