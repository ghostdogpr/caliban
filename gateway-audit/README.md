# Caliban Federation Gateway Audit adapter

This non-published project runs the pinned Federation Gateway Audit against Caliban's native, code-first gateway composition. For each upstream suite, the adapter fetches the upstream `subgraphs` descriptions, passes their authored SDL and endpoints directly to `Gateway.compose`, and serves the resulting `GatewayInterpreter`. It never consumes or creates a serialized supergraph artifact.

The canonical upstream repository, reviewed commit, and review date live in [`upstream.env`](upstream.env). CI runs the
unmodified upstream reporter and requires every reported case to pass. `verify-results.sh` rejects failures, duplicate
cases, and missing or inconsistent result summaries.

## Prepare

Install Node.js, sbt, and a JDK. Then check out the pinned upstream repository:

```sh
CALIBAN_ROOT=/path/to/caliban
. "$CALIBAN_ROOT/gateway-audit/upstream.env"
git clone "$FEDERATION_GATEWAY_AUDIT_REPOSITORY" /path/to/federation-gateway-audit
git -C /path/to/federation-gateway-audit checkout "$FEDERATION_GATEWAY_AUDIT_REVISION"
npm --prefix /path/to/federation-gateway-audit ci --ignore-scripts
```

## Run

Build the adapter, then run the commands from the upstream checkout:

```sh
"$CALIBAN_ROOT/gateway-audit/install.sh"
cd /path/to/federation-gateway-audit
npm start -- test \
  --cwd "$CALIBAN_ROOT/gateway-audit" \
  --run-script ./run.sh \
  --graphql http://127.0.0.1:4000/graphql \
  --healthcheck http://127.0.0.1:4000/health \
  --write "$CALIBAN_ROOT/gateway-audit/results.txt"
"$CALIBAN_ROOT/gateway-audit/verify-results.sh" "$CALIBAN_ROOT/gateway-audit/results.txt"
```

## Adapter settings

The adapter serves `/graphql` and `/health` on port 4000.

| Variable | Default | Effect |
| --- | --- | --- |
| `FEDERATION_GATEWAY_AUDIT_URL` | `http://127.0.0.1:4200` | Where the adapter fetches each suite's subgraph descriptions |
