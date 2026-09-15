# Caliban Federation Gateway Audit adapter

This non-published project runs the pinned Federation Gateway Audit against Caliban's native, code-first gateway composition. For each upstream suite, the adapter fetches the upstream `subgraphs` descriptions, passes their authored SDL and endpoints directly to `Gateway.compose`, and serves the resulting `GatewayInterpreter`. It never consumes or creates a serialized supergraph artifact.

The canonical upstream repository, reviewed commit, and review date live in [`upstream.env`](upstream.env). CI runs the
unmodified upstream reporter and requires every reported case to pass. `verify-results.sh` rejects failures, duplicate
cases, and missing or inconsistent result summaries.

To run the audit locally, set `CALIBAN_ROOT` to this repository and run the commands from a checkout of the pinned
upstream repository:

```sh
CALIBAN_ROOT=/path/to/caliban
. "$CALIBAN_ROOT/gateway-audit/upstream.env"
"$CALIBAN_ROOT/gateway-audit/install.sh"
npm start -- test \
  --cwd "$CALIBAN_ROOT/gateway-audit" \
  --run-script ./run.sh \
  --graphql http://127.0.0.1:4000/graphql \
  --healthcheck http://127.0.0.1:4000/health \
  --write "$CALIBAN_ROOT/gateway-audit/results.txt"
"$CALIBAN_ROOT/gateway-audit/verify-results.sh" "$CALIBAN_ROOT/gateway-audit/results.txt"
```
